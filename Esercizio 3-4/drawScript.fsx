(* REMO ANDREOLI 2018*)

#load "lwc.fsx"
open System.Windows.Forms
open System.Drawing

open Lwc

type DragType =
    | Move //muovi l'intera curva nel mondo
    | Edit //riposiziona un punto per modificare la curva

type ButtonType = Up | Down | Left | Right | RotateLeft | RotateRight | ZoomIn | ZoomOut | Spawn | Merge | Delete | Mode | Animation | NoType

type MyButton() =
    inherit LWControl()
    let mutable text = ""
    let mutable functionality = NoType
    let mutable pressed = false //controlla se il pulsante e' stato premuto

    member this.Pressed
        with get() = pressed
        and set(v) = pressed <- v

    member this.Functionality
        with get() = functionality
        and set(v) = functionality <- v
    
    member this.Text
        with get() = text
        and set(v) = text <- v
    
    override this.OnMouseDown e =
        base.OnMouseDown(e)
        pressed <- true
    
    override this.OnMouseUp e =
        base.OnMouseUp(e)
        pressed <- false

    override this.OnPaint e =
        let parent = this.Parent
        let g = e.Graphics
        let r = RectangleF(this.Position, this.Size) |> RectF2Rect
        g.DrawRectangle(Pens.Red, r)
        let ssz = g.MeasureString(text, parent.Font)
        let p = this.Position
        let sz = this.Size
        let sx, sy = p.X + (sz.Width - ssz.Width) / 2.f, p.Y + (sz.Height - ssz.Height) / 2.f
        g.DrawString(text, parent.Font, Brushes.Red, PointF(sx, sy))

type MyBezier() =
    inherit LWControl()
    let mutable selected = false //controlla se la curva e' selezionata
    let mutable selectedHandleIndex = -1 //indice dell'handle
    let mutable offsetDrag = PointF()
    let mutable points  = Array.empty<PointF>
    let handleSize = 3.f

    let mutable doAnimation = false //controlla se fare l'animazione
    let mutable nSet = 1 //da quante bezier cubiche e' formata (2 set sono due bezier= p[0-3]: prima bezier, p[4-7]: seconda bezier )
    let mutable currentSet = 1 //quale set e' percorso dall'animazione in questo istante
    let step = 1.f/16.f
    let mutable t = 0.f //dove mi trovo sulla curva al tempo t (t compreso tra 0 e 1)

    //calcola il punto in cui ci troviamo su una curva cubica al tempo t
    let pointOnBezier (t:float32)  (points: PointF[]) (set: int) : PointF =
        let idx = (set*3) + 1
        let i = 1.f-t
        let x = points.[idx - 4].X*(i*i*i) + 3.f*points.[idx - 3].X*t*(i*i) + 3.f*points.[idx - 2].X*(t*t)*(i) + points.[idx - 1].X*(t*t*t)
        let y = points.[idx - 4].Y*(i*i*i) + 3.f*points.[idx - 3].Y*t*(i*i) + 3.f*points.[idx - 2].Y*(t*t)*(i) + points.[idx - 1].Y*(t*t*t)
        PointF(x,y)

    //funzione per verificare se p(coords cursore) sta in h(coords handle)
    member this.handleHitTest (p:PointF) (h:PointF) =
        let sqr v = v*v
        let x = p.X - h.X
        let y = p.Y - h.Y
        sqr x + sqr y <= sqr handleSize
    
    member this.NSet
        with get() = nSet
        and set(v) = nSet <- v

    member this.Selected 
        with get() = selected
        and set(v) = selected <- v

    member this.Points
        with get() = points
        and set(v) = points <- v
    
    member this.DoAnimation
        with get() = doAnimation
        and set(v) = doAnimation <- v

    member this.SelectedHandleIndex
        with get() = selectedHandleIndex
        and set(v) = selectedHandleIndex <- v

    member this.SetAnimation() =
            if doAnimation <> true then
                currentSet <- 1
                doAnimation <- true

    member this.ResetAnimation(toggle) = //resetta l'animazione e, eventualmente, la stoppa
            t <- 0.f
            currentSet <- 1
            doAnimation <- toggle

    member this.setHandleOffset(idh:int, l: PointF) =
        let p = points.[idh]
        offsetDrag <- PointF(p.X - l.X, p.Y - l.Y)
    
    //modifica la curva dal punto selezionato
    member this.updateCurveFrom(idh:int, l: PointF) =
          points.[idh] <- PointF(l.X + offsetDrag.X, l.Y + offsetDrag.Y)
    
    //trasla la curva dal punto selezionato
    member this.moveCurveFrom(idh: int, l: PointF) =
        let p = points.[idh]
        for i in 0..points.Length-1 do
            let dx = points.[i].X - p.X
            let dy = points.[i].Y - p.Y
            points.[i] <- PointF(l.X + offsetDrag.X + dx, l.Y + offsetDrag.Y + dy)
   
    //verifica se una maniglia e' stata selezionata
    override this.HitTest p =
        let h = points |> Array.tryFindIndex(fun n -> this.handleHitTest p n)
        match h with
            | Some idx -> 
                        selectedHandleIndex <- idx
                        true
            | None -> 
                        selectedHandleIndex <- -1
                        false
    
    override this.OnPaint e =
        let g = e.Graphics
        let p = this.Position
        let mutable color = if selected = true then Pens.Gold else Pens.Black
        if points.Length = 0 then
            points <- [| p; PointF(p.X+10.f, p.Y+10.f); PointF(p.X+20.f, p.Y+20.f); PointF(p.X+30.f, p.Y+30.f) |]
        
        //disegno la bezier
        g.DrawBeziers(color, points)

        //se avviene una animazione, allora disegno anche la pallina, senno' disegno le maniglie per l'edit/move mode
        if doAnimation = true then
            if t <= 1.f then
                g.FillEllipse(Brushes.Blue, (pointOnBezier t points currentSet).X-handleSize, (pointOnBezier t points currentSet).Y-handleSize, 2.f*handleSize, 2.f*handleSize)
                t <- t + step
            else 
                if currentSet < nSet then currentSet <- currentSet + 1; t <- 0.f
                else this.ResetAnimation(true)
        
        else
            for i in 0..(points.Length-1) do
                g.DrawEllipse(Pens.Red, points.[i].X-handleSize, points.[i].Y-handleSize, 2.f*handleSize, 2.f*handleSize)

type BezierEditor() as this = 
    inherit LWContainer()
    let mutable dragMode = Move 
    let mutable beziers = new ResizeArray<MyBezier>()
    let mutable clientCenter = Point(this.ClientSize.Width/2, this.ClientSize.Height/2) //centro corrente
    let moveTimer = new Timer(Interval=100) //intervallo pressione pulsanti virtuali
    let animTimer = new Timer(Interval=100) //intervallo di esecuzione animazione

    let buttons = [|
        new MyButton(Functionality=Up, Position=PointF(30.f, 0.f), Parent=this, Size=SizeF(30.f, 30.f),Text="W")
        new MyButton(Functionality=Left, Position=PointF(0.f, 30.f), Parent=this, Size=SizeF(30.f, 30.f),Text="A")
        new MyButton(Functionality=Down, Position=PointF(30.f, 30.f), Parent=this, Size=SizeF(30.f, 30.f),Text="S")
        new MyButton(Functionality=Right, Position=PointF(60.f, 30.f), Parent=this, Size=SizeF(30.f, 30.f),Text="D")
        new MyButton(Functionality=RotateLeft, Position=PointF(0.f, 0.f), Parent=this, Size=SizeF(30.f, 30.f),Text="Q")
        new MyButton(Functionality=RotateRight, Position=PointF(60.f, 0.f), Parent=this, Size=SizeF(30.f, 30.f),Text="E")
        new MyButton(Functionality=ZoomIn, Position=PointF(60.f, 60.f), Parent=this, Size=SizeF(30.f, 30.f),Text="X")
        new MyButton(Functionality=ZoomOut, Position=PointF(0.f, 60.f), Parent=this, Size=SizeF(30.f, 30.f),Text="Z")
        new MyButton(Functionality=Spawn, Position=PointF(120.f, 0.f), Parent=this, Size=SizeF(40.f, 30.f), Text="Spawn")
        new MyButton(Functionality=Merge, Position=PointF(160.f, 0.f), Parent=this, Size=SizeF(40.f, 30.f), Text="Merge")
        new MyButton(Functionality=Delete, Position=PointF(200.f, 0.f), Parent=this, Size=SizeF(40.f, 30.f), Text="Del")
        new MyButton(Functionality=Mode, Position=PointF(240.f, 0.f), Parent=this, Size=SizeF(40.f, 30.f), Text="Move")
        new MyButton(Functionality=Animation, Position=PointF(30.f, 60.f), Parent=this, Size=SizeF(30.f, 30.f), Text="!!!")
    |]
    let n = buttons.Length

    let transformP (m:Drawing2D.Matrix) (p:Point) =
        let a = [| PointF(single p.X, single p.Y) |]
        m.TransformPoints(a)
        a.[0]
         
    do
        buttons |> Array.iter(fun b -> this.LWControls.Add(b))
        buttons |> Array.iter(fun b ->
                                   b.MouseDown.Add(fun _ -> moveTimer.Start(); this.Invalidate())
                                   b.MouseUp.Add(fun _ -> moveTimer.Stop())
                                   match b.Functionality with
                                            | Up -> b.MouseDown.Add(fun _ ->  this.Transform.RelativeTranslate(0.f, 10.f, this.Round))
                                            | Left -> b.MouseDown.Add(fun _ -> this.Transform.RelativeTranslate(10.f, 0.f, this.Round))
                                            | Down -> b.MouseDown.Add(fun _ -> this.Transform.RelativeTranslate(0.f, -10.f, this.Round))
                                            | Right -> b.MouseDown.Add(fun _ -> this.Transform.RelativeTranslate(-10.f, 0.f, this.Round))
                                            | RotateLeft -> b.MouseDown.Add(fun _ -> this.Transform.RotateAt(10.f, clientCenter); this.Round <- this.Round + 10.f)
                                            | RotateRight -> b.MouseDown.Add(fun _ -> this.Transform.RotateAt(-10.f, clientCenter); this.Round <- this.Round - 10.f)
                                            | ZoomIn -> b.MouseDown.Add(fun _ -> this.Transform.ScaleAt(1.1f, 1.1f, clientCenter))
                                            | ZoomOut -> b.MouseDown.Add(fun _ -> this.Transform.ScaleAt(1.f/1.1f, 1.f/1.1f, clientCenter))
                                            | Spawn -> b.MouseDown.Add(fun _ -> this.NewBezier())
                                            | Merge -> b.MouseDown.Add(fun _ -> this.MergeBeziers())               
                                            | Delete -> b.MouseDown.Add(fun _ -> this.DeleteSelectedB())
                                            | Mode -> b.MouseDown.Add(fun _ -> this.ToggleMode(b))
                                            | Animation -> b.MouseDown.Add(fun _ -> if animTimer.Enabled <> true then animTimer.Start()
                                                                                    else animTimer.Stop(); this.StopAnimation(); this.Invalidate())
                                            | NoType -> printfn "error"
                                    
        )

        animTimer.Tick.Add(fun _ -> this.StartAnimation(); this.Invalidate())

        moveTimer.Tick.Add(fun _ ->
            let pressed = buttons |> Array.tryFind(fun b -> b.Pressed)
            match pressed with
                | Some b -> 
                    match b.Functionality with
                        | Up -> this.Transform.RelativeTranslate(0.f, 10.f, this.Round)
                        | Left -> this.Transform.RelativeTranslate(10.f, 0.f, this.Round)
                        | Down -> this.Transform.RelativeTranslate(0.f, -10.f, this.Round)
                        | Right -> this.Transform.RelativeTranslate(-10.f, 0.f, this.Round)
                        | RotateLeft -> this.Transform.RotateAt(10.f, clientCenter); this.Round <- this.Round + 10.f
                        | RotateRight -> this.Transform.RotateAt(-10.f, clientCenter); this.Round <- this.Round - 10.f
                        | ZoomIn -> this.Transform.ScaleAt(1.1f, 1.1f, clientCenter)
                        | ZoomOut -> this.Transform.ScaleAt(1.f/1.1f, 1.f/1.1f, clientCenter)
                        | Merge -> this.MergeBeziers()
                        | Delete -> this.DeleteSelectedB()
                        | _ -> ()
                    this.Invalidate()
                | None -> ()
        )
    done

    //come parametro il bottome stesso cosi' posso cambiargli il testo
    member this.ToggleMode(b:MyButton) =
        if dragMode = Move then 
            dragMode <- Edit
            b.Text <- "Edit"
        else 
            dragMode <- Move
            b.Text <- "Move"

    //fa un merge tra la curva selezionata e un'altra curva con cui collide nei punti di controllo (inizio e fine curva)
    member this.MergeBeziers() =
         let selectedB = beziers |> Seq.tryFind(fun b -> b.Selected)
         match selectedB with
            | Some sB ->
                let sBorderPoints = [| sB.Points.[0]; sB.Points.[sB.Points.Length-1] |] //punti di inizio/fine della bezier selezionata
                let startMerge = sB.handleHitTest sBorderPoints.[0] //funzione che verifica se la bezier selezionata deve essere fusa dal punto iniziale
                let endMerge = sB.handleHitTest sBorderPoints.[1] //funzione che verifica se la bezier deve essere fusa dal punto finale
                
                //merge con l'inizio della curva selezionata e l'inizio dell' i-esima curva (selezionata esclusa)
                let start2Start = beziers |> Seq.tryFind(fun x -> sB <> x && (startMerge x.Points.[0]))
                match start2Start with
                    | Some iB ->
                        let idx = beziers |> Seq.findIndex((=) iB)
                        let newPoints = Array.rev (iB.Points |> Array.filter(fun x -> x <> iB.Points.[0]))
                        let oldPoints = sB.Points
                        let mergedPoints = Array.append newPoints oldPoints
                        sB.Points <- mergedPoints
                        sB.NSet <- sB.NSet + iB.NSet
                        this.DeleteBezierAt(idx)
                    | None -> ()
                
                //merge con l'inizio della curva selezionata e la fine dell' i-esima curva (selezionata esclusa)
                let start2End = beziers |> Seq.tryFind(fun x -> sB <> x && (startMerge x.Points.[x.Points.Length-1]))
                match start2End with
                    | Some iB ->
                        let idx = beziers |> Seq.findIndex((=) iB)
                        let newPoints = iB.Points |> Array.filter(fun x -> x <> iB.Points.[iB.Points.Length-1])
                        let oldPoints = sB.Points
                        let mergedPoints = Array.append newPoints oldPoints
                        sB.Points <- mergedPoints
                        sB.NSet <- sB.NSet + iB.NSet
                        this.DeleteBezierAt(idx)
                    | None -> ()
                
                 //merge con la fine della curva selezionata e l'inizio dell' i-esima curva (selezionata esclusa)
                let end2Start = beziers |> Seq.tryFind(fun x -> sB <> x && (endMerge x.Points.[0]))
                match end2Start with
                    | Some iB ->
                        let idx = beziers |> Seq.findIndex((=) iB)
                        let newPoints = iB.Points |> Array.filter(fun x -> x <> iB.Points.[0])
                        let oldPoints = sB.Points
                        let mergedPoints = Array.append oldPoints newPoints
                        sB.Points <- mergedPoints
                        sB.NSet <- sB.NSet + iB.NSet
                        this.DeleteBezierAt(idx)
                    | None -> ()
                
                 //merge con la fine della curva selezionata e la fine dell' i-esima curva (selezionata esclusa)
                let end2End = beziers |> Seq.tryFind(fun x -> sB <> x && (endMerge x.Points.[x.Points.Length-1]))
                match end2End with    
                    | Some iB ->
                        let idx = beziers |> Seq.findIndex((=) iB)
                        let newPoints = Array.rev (iB.Points |> Array.filter(fun x -> x <> iB.Points.[iB.Points.Length-1]))
                        let oldPoints = sB.Points
                        let mergedPoints = Array.append oldPoints newPoints
                        sB.Points <- mergedPoints
                        sB.NSet <- sB.NSet + iB.NSet
                        this.DeleteBezierAt(idx)
                    | None -> ()
                
                printfn "selected e' formata da %d curve" sB.NSet
            | _  -> () 

     member this.DeleteBezierAt(id) =
        if id <> -1 then
            this.LWControls.RemoveAt(id+n) //i primi n elementi sono bottoni
            beziers.RemoveAt(id)
        else printfn "nessuna bezier da rimuovere"

     member this.DeleteSelectedB() =
        let selectedBezierId = beziers |> Seq.tryFindIndex(fun b -> b.Selected)
        match selectedBezierId with
            | Some id ->
                this.LWControls.RemoveAt(id+n) //i primi n elementi sono bottoni
                beziers.RemoveAt(id)
                if beziers.Count <> 0 then beziers.[beziers.Count-1].Selected <- true //se ci sono altre bezier, seleziono l'ultima creata
                printfn "curve rimanenti: %d" beziers.Count
            | None -> printfn "nessuna bezier da rimuovere"

     member this.StartAnimation() =
        let animated = beziers |> Seq.tryFind(fun b -> b.DoAnimation && b.Selected = false) //fermo qualunque altra animazione in corso
        match animated with
            | Some aB -> aB.ResetAnimation(false)
            | None ->
                let selectedBezier = beziers |> Seq.tryFind( fun b -> b.Selected && b.DoAnimation = false) //curva selezionata
                match selectedBezier with
                    | Some b -> b.SetAnimation()
                    | None -> ()

     member this.StopAnimation() =
        let selectedBezier = beziers |> Seq.tryFind( fun b -> b.Selected) //curva selezionata
        match selectedBezier with
            | Some b -> 
                b.ResetAnimation(false) //resetta l'animazione e la ferma
            | None -> ()

     //disegno la nuova curva al centro della vista
     member this.NewBezier() =
        let l = transformP this.Transform.V2W clientCenter
        let bz = MyBezier(Position=l, Parent=this, CoordinateType=World)
        beziers.Add(bz)
        this.LWControls.Add(bz)

    override this.OnResize e =
        clientCenter <- Point(this.ClientSize.Width/2, this.ClientSize.Height/2)
        base.OnResize(e)

    override this.OnMouseDown e = 
        base.OnMouseDown(e) //setta l'handle selezionato della curva cliccata
        let selectedBezier = beziers |> Seq.tryFind( fun b -> b.SelectedHandleIndex <> -1 && b.DoAnimation = false) //cerca tale curva
        match selectedBezier with
            | Some b -> 
                let idx = beziers |> Seq.findIndex( fun x -> x = b)
                let l = transformP this.Transform.V2W e.Location 
                b.Selected <- true
                b.setHandleOffset(b.SelectedHandleIndex, l)
                beziers |> Seq.iter(fun x -> if x <> b then x.Selected <- false) //setto a false qualunque altra curva se in precedenza
                printfn "selezionata curva index (%d) da handle (%d) e formata da (%d) punti" idx b.SelectedHandleIndex b.Points.Length
            | None -> ()
        this.Invalidate()

    override this.OnMouseMove e =
        base.OnMouseMove(e)
        let selectedBezier = beziers|> Seq.tryFind( fun b -> b.SelectedHandleIndex <> -1 && b.DoAnimation = false)
        match selectedBezier with
            | Some b ->
                let l = transformP this.Transform.V2W e.Location
                if dragMode = Move then b.moveCurveFrom(b.SelectedHandleIndex, l)
                else  b.updateCurveFrom(b.SelectedHandleIndex, l)
                this.Invalidate()
            | None -> ()
    
    override this.OnMouseUp e =
        base.OnMouseUp(e)
        let selectedBezier = beziers|> Seq.tryFind( fun b -> b.SelectedHandleIndex <> -1)
        match selectedBezier with
                    | Some b ->
                        b.SelectedHandleIndex <- -1
                        this.Invalidate()
                    | None -> ()
        

let e = new BezierEditor(Dock = DockStyle.Fill)
let f = new Form()
f.Controls.Add(e)
f.Show()
