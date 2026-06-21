(* test_vec.sml -- Vec2/Vec3/Vec4 suite *)

structure VecTests =
struct
  structure G = Glm
  open Support

  fun run () =
    let
      val _ = Harness.section "Vec2"
      val a = G.Vec2.v (1.0, 2.0)
      val b = G.Vec2.v (3.0, 4.0)
      val () = checkClose "Vec2.x" (1.0, G.Vec2.x a)
      val () = checkClose "Vec2.y" (2.0, G.Vec2.y a)
      val () = checkVec2 "Vec2.add" (G.Vec2.v (4.0, 6.0), G.Vec2.add (a, b))
      val () = checkVec2 "Vec2.sub" (G.Vec2.v (~2.0, ~2.0), G.Vec2.sub (a, b))
      val () = checkVec2 "Vec2.scale" (G.Vec2.v (2.0, 4.0), G.Vec2.scale (2.0, a))
      val () = checkVec2 "Vec2.neg" (G.Vec2.v (~1.0, ~2.0), G.Vec2.neg a)
      val () = checkClose "Vec2.dot" (11.0, G.Vec2.dot (a, b))
      val () = checkClose "Vec2.length (3,4)=5" (5.0, G.Vec2.length (G.Vec2.v (3.0, 4.0)))
      val () = checkClose "Vec2.lengthSq (3,4)=25" (25.0, G.Vec2.lengthSq (G.Vec2.v (3.0, 4.0)))
      val () = checkVec2 "Vec2.normalize axis = itself"
                 (G.Vec2.v (1.0, 0.0), G.Vec2.normalize (G.Vec2.v (5.0, 0.0)))
      val () = checkClose "Vec2.normalize is unit length"
                 (1.0, G.Vec2.length (G.Vec2.normalize b))
      val () = checkVec2 "Vec2.lerp t=0" (a, G.Vec2.lerp (a, b, 0.0))
      val () = checkVec2 "Vec2.lerp t=1" (b, G.Vec2.lerp (a, b, 1.0))
      val () = checkVec2 "Vec2.lerp t=0.5"
                 (G.Vec2.v (2.0, 3.0), G.Vec2.lerp (a, b, 0.5))
      val () = Harness.check "Vec2.approx within tol"
                 (G.Vec2.approx 1E~6 (a, G.Vec2.v (1.0 + 1E~9, 2.0)))
      val () = Harness.check "Vec2.approx outside tol = false"
                 (not (G.Vec2.approx 1E~9 (a, G.Vec2.v (1.1, 2.0))))
      (* edge: zero-vector normalize does not divide by zero *)
      val () = checkVec2 "Vec2.normalize zero = zero"
                 (G.Vec2.zero, G.Vec2.normalize G.Vec2.zero)

      val _ = Harness.section "Vec3"
      val x = G.Vec3.v (1.0, 0.0, 0.0)
      val y = G.Vec3.v (0.0, 1.0, 0.0)
      val z = G.Vec3.v (0.0, 0.0, 1.0)
      val () = checkClose "Vec3.dot orthonormal = 0" (0.0, G.Vec3.dot (x, y))
      val () = checkClose "Vec3.dot self = 1" (1.0, G.Vec3.dot (x, x))
      val () = checkVec3 "Vec3.cross x y = z" (z, G.Vec3.cross (x, y))
      val () = checkVec3 "Vec3.cross y z = x" (x, G.Vec3.cross (y, z))
      val () = checkVec3 "Vec3.cross z x = y" (y, G.Vec3.cross (z, x))
      val u = G.Vec3.v (1.0, 2.0, 2.0)
      val () = checkClose "Vec3.length (1,2,2)=3" (3.0, G.Vec3.length u)
      val () = checkVec3 "Vec3.add"
                 (G.Vec3.v (1.0, 1.0, 1.0), G.Vec3.add (x, G.Vec3.add (y, z)))
      val () = checkClose "Vec3.dist" (Math.sqrt 3.0,
                 G.Vec3.dist (G.Vec3.v (0.0,0.0,0.0), G.Vec3.v (1.0,1.0,1.0)))
      val () = checkVec3 "Vec3.normalize zero = zero"
                 (G.Vec3.zero, G.Vec3.normalize G.Vec3.zero)

      val _ = Harness.section "Vec4"
      val p = G.Vec4.v (1.0, 2.0, 3.0, 4.0)
      val q = G.Vec4.v (5.0, 6.0, 7.0, 8.0)
      val () = checkClose "Vec4.dot" (70.0, G.Vec4.dot (p, q))
      val () = checkVec4 "Vec4.add"
                 (G.Vec4.v (6.0, 8.0, 10.0, 12.0), G.Vec4.add (p, q))
      val () = checkClose "Vec4.length (1,0,0,0)=1"
                 (1.0, G.Vec4.length (G.Vec4.v (1.0,0.0,0.0,0.0)))
      val () = checkVec4 "Vec4.normalize zero = zero"
                 (G.Vec4.zero, G.Vec4.normalize G.Vec4.zero)
    in
      ()
    end
end
