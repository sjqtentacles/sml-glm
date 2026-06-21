(* test_quat.sml -- Quat suite *)

structure QuatTests =
struct
  structure G = Glm
  open Support

  fun run () =
    let
      val _ = Harness.section "Quat"
      (* id rotates nothing *)
      val v = G.Vec3.v (1.0, 2.0, 3.0)
      val () = checkVec3 "Quat.id rotates nothing" (v, G.Quat.rotateV (G.Quat.id, v))

      (* fromAxisAngle + rotateV equals matrix rotation *)
      val ang = G.radians 73.0
      val axis = G.Vec3.normalize (G.Vec3.v (1.0, 2.0, 3.0))
      val q = G.Quat.fromAxisAngle (axis, ang)
      val byQuat = G.Quat.rotateV (q, v)
      val byMat = G.Mat4.transformDir (G.Mat4.rotate (ang, axis), v)
      val () = checkVec3 "Quat.rotateV = Mat4.rotate" (byMat, byQuat)

      (* rotateZ 90: x -> y *)
      val qz = G.Quat.fromAxisAngle (G.Vec3.v (0.0,0.0,1.0), G.radians 90.0)
      val () = checkVec3 "Quat rotateZ 90 maps x->y"
                 (G.Vec3.v (0.0, 1.0, 0.0), G.Quat.rotateV (qz, G.Vec3.v (1.0,0.0,0.0)))

      (* mul composition order: (q2 then q1) = mul(q1,q2) applied *)
      val qx = G.Quat.fromAxisAngle (G.Vec3.v (1.0,0.0,0.0), G.radians 90.0)
      val composed = G.Quat.mul (qz, qx)   (* apply qx first, then qz *)
      val seq = G.Quat.rotateV (qz, G.Quat.rotateV (qx, v))
      val () = checkVec3 "Quat.mul composition order" (seq, G.Quat.rotateV (composed, v))

      (* conj of unit quat is its inverse rotation *)
      val backToV = G.Quat.rotateV (G.Quat.conj q, G.Quat.rotateV (q, v))
      val () = checkVec3 "Quat.conj inverts rotation" (v, backToV)

      (* normalize gives unit length *)
      val () = checkClose "Quat.normalize unit length"
                 (1.0, G.Quat.length (G.Quat.normalize (G.Quat.quat (1.0,2.0,3.0,4.0))))

      (* slerp endpoints exact *)
      val q1 = G.Quat.fromAxisAngle (G.Vec3.v (0.0,1.0,0.0), G.radians 0.0)
      val q2 = G.Quat.fromAxisAngle (G.Vec3.v (0.0,1.0,0.0), G.radians 90.0)
      val () = checkQuat "Quat.slerp t=0 = q1" (q1, G.Quat.slerp (q1, q2, 0.0))
      val () = checkQuat "Quat.slerp t=1 = q2" (q2, G.Quat.slerp (q1, q2, 1.0))
      val () = checkClose "Quat.slerp midpoint unit length"
                 (1.0, G.Quat.length (G.Quat.slerp (q1, q2, 0.5)))
      (* slerp midpoint = 45 deg rotation *)
      val mid = G.Quat.slerp (q1, q2, 0.5)
      val mid45 = G.Quat.fromAxisAngle (G.Vec3.v (0.0,1.0,0.0), G.radians 45.0)
      val () = checkVec3 "Quat.slerp midpoint rotates like 45deg"
                 (G.Quat.rotateV (mid45, G.Vec3.v (1.0,0.0,0.0)),
                  G.Quat.rotateV (mid, G.Vec3.v (1.0,0.0,0.0)))

      (* edge: slerp of (q, q) is q *)
      val () = checkQuat "Quat.slerp (q,q) = q" (q1, G.Quat.slerp (q1, q1, 0.5))
      (* edge: near-antipodal stays unit length (numerical stability) *)
      val qa = G.Quat.quat (1.0, 0.0, 0.0, 0.0)
      val qb = G.Quat.quat (~1.0, 1E~7, 0.0, 0.0)
      val () = checkClose "Quat.slerp near-antipodal unit length"
                 (1.0, G.Quat.length (G.Quat.slerp (qa, qb, 0.5)))

      (* toMat4 / fromMat3 round-trip on a known rotation *)
      val m4 = G.Quat.toMat4 q
      val dirByMat = G.Mat4.transformDir (m4, v)
      val () = checkVec3 "Quat.toMat4 matches rotateV" (byQuat, dirByMat)
      (* build Mat3 rotation, recover quaternion, compare rotation effect *)
      val m3 = G.Mat3.fromRows
                 (G.Vec3.v (0.0, ~1.0, 0.0),
                  G.Vec3.v (1.0,  0.0, 0.0),
                  G.Vec3.v (0.0,  0.0, 1.0))   (* rotateZ 90 *)
      val qFrom = G.Quat.fromMat3 m3
      val () = checkVec3 "Quat.fromMat3 recovers rotation"
                 (G.Vec3.v (0.0, 1.0, 0.0),
                  G.Quat.rotateV (qFrom, G.Vec3.v (1.0, 0.0, 0.0)))
    in
      ()
    end
end
