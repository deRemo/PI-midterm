(* REMO ANDREOLI 2018*)

open System.Windows.Forms
open System.Drawing

type W2V() =
    let mutable w2v = new Drawing2D.Matrix()
    let mutable v2w = new Drawing2D.Matrix()
    
    member this.TransformPoint (p:Point) =
        let toPointF (p:Point) = PointF(single p.X, single p.Y)
        let a = [| p |> toPointF |]
        v2w.TransformPoints(a)
        a.[0]

    member this.Translate(tx, ty) =
        w2v.Translate(tx, ty)
        v2w.Translate(-tx, -ty, Drawing2D.MatrixOrder.Append)

    member this.Rotate(a) =
        w2v.Rotate(a)
        v2w.Rotate(-a, Drawing2D.MatrixOrder.Append)

    member this.Scale(sx, sy) =
        w2v.Scale(sx, sy)
        v2w.Scale(1.f/sx, 1.f/sy, Drawing2D.MatrixOrder.Append)
   
    //ruota in relazione a center
    member this.RotateAt(x, center: Point) =
        let cw = center |> this.TransformPoint
        w2v.RotateAt(x, cw)
        v2w.RotateAt(-x, cw, Drawing2D.MatrixOrder.Append)
    
     //zoom in/out in relazione a center
    member this.ScaleAt(sx, sy, center:Point) =
        let cw = center |> this.TransformPoint
        this.Scale(sx, sy)
        let cwp = center |> this.TransformPoint
        this.Translate(cwp.X - cw.X, cwp.Y - cw.Y)
        
    member this.RelativeTranslate(x, y, round) =
        this.Rotate(-round)
        this.Translate(x, y)
        this.Rotate(round)

    member this.W2V 
        with get() = w2v
        and set(v:Drawing2D.Matrix) = w2v <- v.Clone()

    member this.V2W 
        with get() = v2w
        and set(v:Drawing2D.Matrix) = v2w <- v.Clone()

let Rect2RectF (r:Rectangle) =
  RectangleF(single r.X, single r.Y, single r.Width, single r.Height)

let RectF2Rect (r:RectangleF) =
  Rectangle(int r.X, int r.Y, int r.Width, int r.Height)

type CoordinateType = View | World

type LWControl() =
    let mutable coordinates = View

    let mutable position = PointF()
    let mutable size = SizeF()
    let mutable parent : Control = null

    let mousedownevt = new Event<MouseEventArgs>()
    let mousemoveevt = new Event<MouseEventArgs>()
    let mouseupevt = new Event<MouseEventArgs>()

    member this.CoordinateType
        with get() = coordinates
        and set(v) = coordinates <- v
 
    member this.Position
        with get() = position
        and set(v) = position <- v

    member this.Size
        with get() = size
        and set(v) = size <- v
   
    member this.Parent 
        with get() = parent
        and set(v) = parent <- v

    member this.MouseDown = mousedownevt.Publish
    member this.MouseUp = mouseupevt.Publish

    abstract OnMouseDown : MouseEventArgs -> unit
    default this.OnMouseDown e = mousedownevt.Trigger(e)

    abstract OnMouseMove : MouseEventArgs -> unit
    default this.OnMouseMove e = mousemoveevt.Trigger(e)

    abstract OnMouseUp : MouseEventArgs -> unit
    default this.OnMouseUp e = mouseupevt.Trigger(e)

    abstract OnPaint : PaintEventArgs -> unit
    default this.OnPaint e = ()

    abstract HitTest : PointF -> bool
    default this.HitTest p =
        (new RectangleF(position, size)).Contains(p)

type LWContainer() as this  =
    inherit UserControl()
    
    let transform = W2V()
    let controls = ResizeArray<LWControl>()
    let mutable round = 0.f //tiene traccia della rotazione della vista
    let transformPoint (m:Drawing2D.Matrix) (p:PointF) =
        let pts = [| p |]
        m.TransformPoints(pts)
        pts.[0]
    
    do this.SetStyle(ControlStyles.AllPaintingInWmPaint ||| ControlStyles.OptimizedDoubleBuffer, true) //per evitare flickering

    member this.Transform with get() = transform
    member this.LWControls with get() = controls

    member this.Round 
        with get() = round
        and set(v) = round <- v

    //ridisegno tutto se faccio un resize della finestra
    override this.OnResize e =
        this.Invalidate()

    override this.OnMouseDown e =
        let p = PointF(single e.X, single e.Y)
        let controlsView = controls |> Seq.filter (fun c -> c.CoordinateType = View)
        
        match (controlsView |> Seq.tryFind (fun c -> c.HitTest p)) with
            | Some c -> c.OnMouseDown(e)
            | None -> 
                let pw = transformPoint transform.V2W p
                let controlsWorld = controls |> Seq.filter (fun c -> c.CoordinateType = World)
                match (controlsWorld |> Seq.tryFind(fun c -> c.HitTest pw)) with
                    | Some c -> c.OnMouseDown(e)
                    | None -> ()

    override this.OnMouseMove e =
        let p = PointF(single e.X, single e.Y)
        let controlsView = controls |> Seq.filter (fun c -> c.CoordinateType = View)
        match (controlsView |> Seq.tryFind (fun c -> c.HitTest p)) with
            | Some c -> c.OnMouseMove(e)
            | None -> ()

    override this.OnMouseUp e =
        let p = PointF(single e.X, single e.Y)
        let controlsView = controls |> Seq.filter (fun c -> c.CoordinateType = View)
        match (controlsView |> Seq.tryFind (fun c -> c.HitTest p)) with
            | Some c -> c.OnMouseUp(e)
            | None -> ()

    override this.OnPaint e =
        let g = e.Graphics
        let t = g.Transform

        g.DrawLine(Pens.Black, this.Width / 2 - 10, this.Height / 2, this.Width / 2 + 10, this.Height / 2)
        g.DrawLine(Pens.Black, this.Width / 2, this.Height / 2 - 10, this.Width / 2, this.Height / 2 + 10)

        g.Transform <- transform.W2V
        
        for idx in (controls.Count - 1) .. -1 .. 0 do
            let c = controls.[idx]
            if c.CoordinateType = World then
                c.OnPaint e
    
        g.Transform <- t

        for idx in (controls.Count - 1) .. -1 .. 0 do
            let c = controls.[idx]
            if c.CoordinateType = View then
                c.OnPaint e
  
    override this.OnKeyDown e =
        let cc = Point(this.ClientSize.Width/2, this.ClientSize.Height/2) //centro del client
        match e.KeyCode with
            | Keys.W ->
                transform.RelativeTranslate(0.f, 10.f, round)
                this.Invalidate()
            | Keys.A -> 
                 transform.RelativeTranslate(10.f, 0.f, round)
                 this.Invalidate()
            | Keys.S ->
                transform.RelativeTranslate(0.f, -10.f, round)
                this.Invalidate()
            | Keys.D ->
                transform.RelativeTranslate(-10.f, 0.f, round)
                this.Invalidate()
            | Keys.Q ->
                round <- round + 10.f
                transform.RotateAt(10.f, cc)
                this.Invalidate()
            | Keys.E ->
                round <- round - 10.f
                transform.RotateAt(-10.f, cc)
                this.Invalidate()
            | Keys.Z ->
                transform.ScaleAt(1.1f, 1.1f, cc)
                this.Invalidate()
            | Keys.X ->
                let cs = this.ClientSize
                transform.ScaleAt(1.f/1.1f, 1.f/1.1f, cc)
                this.Invalidate()
            | _ -> ()
