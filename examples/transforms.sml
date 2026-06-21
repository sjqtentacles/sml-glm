(* sml-glm demo: builds 2D affine transforms as Mat3 (homogeneous coords),
   applies them to an arrow polygon with Mat3.mulV, and rasterizes the result
   over a coordinate grid -> assets/transforms.png. *)

open Glm

fun rgba (r, g, b, a) : Image.rgba8 =
  { r = Word8.fromInt r, g = Word8.fromInt g
  , b = Word8.fromInt b, a = Word8.fromInt a }

val width = 512
val height = 512
val ox = 256
val oy = 256

(* Arrow polygon in local space, centered near the origin, pointing +x. *)
val shape =
  [ (~78.0, ~27.0), (20.0, ~27.0), (20.0, ~64.0), (112.0, 0.0)
  , (20.0, 64.0), (20.0, 27.0), (~78.0, 27.0) ]

fun rot a =
  Mat3.fromRows ( Vec3.v (Math.cos a, ~(Math.sin a), 0.0)
                , Vec3.v (Math.sin a,   Math.cos a,  0.0)
                , Vec3.v (0.0,          0.0,         1.0) )

fun scaleM (sx, sy) =
  Mat3.fromRows ( Vec3.v (sx,  0.0, 0.0)
                , Vec3.v (0.0, sy,  0.0)
                , Vec3.v (0.0, 0.0, 1.0) )

fun xf (m, (x, y)) =
  let val r = Mat3.mulV (m, Vec3.v (x, y, 1.0))
  in (ox + Real.round (Vec3.x r), oy - Real.round (Vec3.y r)) end

(* Draw with a little faux thickness by overlaying a 1px-offset copy. *)
fun drawShape (img, m, color) =
  let
    val pts = map (fn p => xf (m, p)) shape
    val closed = pts @ [hd pts]
    val c1 = Raster.polyline img closed color
    val c2 = Raster.polyline c1 (map (fn (x, y) => (x + 1, y)) closed) color
  in
    Raster.polyline c2 (map (fn (x, y) => (x, y + 1)) closed) color
  end

(* Background grid. *)
val grid = rgba (44, 50, 62, 255)
val axis = rgba (90, 100, 120, 255)
val img0 =
  let
    val c = Raster.blank (width, height) (rgba (22, 25, 31, 255))
    fun vlines (x, c) =
      if x >= width then c
      else vlines (x + 32, Raster.line c { x0 = x, y0 = 0, x1 = x, y1 = height - 1 } grid)
    fun hlines (y, c) =
      if y >= height then c
      else hlines (y + 32, Raster.line c { x0 = 0, y0 = y, x1 = width - 1, y1 = y } grid)
    val c = vlines (0, c)
    val c = hlines (0, c)
    val c = Raster.line c { x0 = ox, y0 = 0, x1 = ox, y1 = height - 1 } axis
    val c = Raster.line c { x0 = 0, y0 = oy, x1 = width - 1, y1 = oy } axis
  in c end

(* A fan of transforms: each is rotate(k * step) composed with a shrink. *)
val palette =
  Vector.fromList
    [ rgba (236, 240, 245, 235), rgba (239,  71, 111, 255)
    , rgba (255, 209,  102, 255), rgba ( 6, 214, 160, 255)
    , rgba ( 17, 138, 178, 255), rgba (131,  56, 236, 255) ]

val img =
  let
    fun step (k, c) =
      if k >= Vector.length palette then c
      else
        let
          val m = Mat3.mul (rot (real k * 0.5), scaleM (1.0 - real k * 0.13, 1.0 - real k * 0.13))
        in
          step (k + 1, drawShape (c, m, Vector.sub (palette, k)))
        end
  in
    step (0, img0)
  end

val () =
  let
    val os = BinIO.openOut "assets/transforms.png"
  in
    BinIO.output (os, Image.encodePng img);
    BinIO.closeOut os;
    print "wrote assets/transforms.png\n"
  end
