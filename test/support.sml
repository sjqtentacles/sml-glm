(* support.sml

   Float-aware assertions layered on top of the exact-equality Harness, plus
   shared fixtures. `eps` is a single shared tolerance so failures are legible
   and identical across MLton and Poly/ML. *)

structure Support =
struct
  structure G = Glm

  val eps = 1E~9

  fun close (a, b) = Real.abs (a - b) <= eps

  fun checkClose name (exp, act) =
    Harness.check name (close (exp, act))

  fun checkVec2 name (ex, ac) =
    Harness.check name (G.Vec2.approx eps (ex, ac))
  fun checkVec3 name (ex, ac) =
    Harness.check name (G.Vec3.approx eps (ex, ac))
  fun checkVec4 name (ex, ac) =
    Harness.check name (G.Vec4.approx eps (ex, ac))
  fun checkMat2 name (ex, ac) =
    Harness.check name (G.Mat2.approx eps (ex, ac))
  fun checkMat3 name (ex, ac) =
    Harness.check name (G.Mat3.approx eps (ex, ac))
  fun checkMat4 name (ex, ac) =
    Harness.check name (G.Mat4.approx eps (ex, ac))
  fun checkQuat name (ex, ac) =
    Harness.check name (G.Quat.approx eps (ex, ac))
end
