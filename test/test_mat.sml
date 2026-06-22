(* test_mat.sml -- Mat3/Mat4 suite *)

structure MatTests =
struct
  structure G = Glm
  open Support

  fun run () =
    let
      val _ = Harness.section "Mat2"
      val m2 = G.Mat2.fromRows (G.Vec2.v (1.0, 2.0), G.Vec2.v (3.0, 4.0))
      val () = checkMat2 "Mat2.mul id left" (m2, G.Mat2.mul (G.Mat2.id, m2))
      val () = checkMat2 "Mat2.mul id right" (m2, G.Mat2.mul (m2, G.Mat2.id))
      val () = checkMat2 "Mat2.transpose involution"
                 (m2, G.Mat2.transpose (G.Mat2.transpose m2))
      val () = checkClose "Mat2.det id = 1" (1.0, G.Mat2.det G.Mat2.id)
      val () = checkClose "Mat2.det known = -2" (~2.0, G.Mat2.det m2)
      val () = checkVec2 "Mat2.mulV id = v"
                 (G.Vec2.v (3.0, ~2.0), G.Mat2.mulV (G.Mat2.id, G.Vec2.v (3.0, ~2.0)))
      (* m2 * (1,1) = row sums *)
      val () = checkVec2 "Mat2.mulV row sums"
                 (G.Vec2.v (3.0, 7.0), G.Mat2.mulV (m2, G.Vec2.v (1.0, 1.0)))
      val () = (case G.Mat2.inverse m2 of
                  SOME inv => checkMat2 "Mat2.inverse: m*inv = id"
                                (G.Mat2.id, G.Mat2.mul (m2, inv))
                | NONE => Harness.check "Mat2.inverse: m*inv = id" false)
      val () = Harness.check "Mat2.inverse singular = NONE"
                 (not (Option.isSome
                   (G.Mat2.inverse
                      (G.Mat2.fromRows (G.Vec2.v (1.0,2.0), G.Vec2.v (2.0,4.0))))))
      (* fromCols is the transpose of fromRows of the same pair *)
      val () = checkMat2 "Mat2.fromCols = transpose fromRows"
                 (G.Mat2.fromCols (G.Vec2.v (1.0,2.0), G.Vec2.v (3.0,4.0)),
                  G.Mat2.transpose
                    (G.Mat2.fromRows (G.Vec2.v (1.0,2.0), G.Vec2.v (3.0,4.0))))

      val _ = Harness.section "Mat3"
      val m = G.Mat3.fromRows (G.Vec3.v (1.0, 2.0, 3.0),
                               G.Vec3.v (4.0, 5.0, 6.0),
                               G.Vec3.v (7.0, 8.0, 10.0))
      val () = checkMat3 "Mat3.mul id left" (m, G.Mat3.mul (G.Mat3.id, m))
      val () = checkMat3 "Mat3.mul id right" (m, G.Mat3.mul (m, G.Mat3.id))
      val () = checkMat3 "Mat3.transpose involution"
                 (m, G.Mat3.transpose (G.Mat3.transpose m))
      val () = checkClose "Mat3.det id = 1" (1.0, G.Mat3.det G.Mat3.id)
      val () = checkClose "Mat3.det known = -3" (~3.0, G.Mat3.det m)
      val () = checkVec3 "Mat3.mulV id = v"
                 (G.Vec3.v (2.0,3.0,5.0), G.Mat3.mulV (G.Mat3.id, G.Vec3.v (2.0,3.0,5.0)))
      (* mulV against hand-computed: m * (1,1,1) = row sums *)
      val () = checkVec3 "Mat3.mulV row sums"
                 (G.Vec3.v (6.0, 15.0, 25.0), G.Mat3.mulV (m, G.Vec3.v (1.0,1.0,1.0)))
      val () = (case G.Mat3.inverse m of
                  SOME inv => checkMat3 "Mat3.inverse: m*inv = id"
                                (G.Mat3.id, G.Mat3.mul (m, inv))
                | NONE => Harness.check "Mat3.inverse: m*inv = id" false)
      (* edge: singular matrix -> NONE *)
      val singular = G.Mat3.fromRows (G.Vec3.v (1.0,2.0,3.0),
                                      G.Vec3.v (2.0,4.0,6.0),
                                      G.Vec3.v (1.0,1.0,1.0))
      val () = Harness.check "Mat3.inverse singular = NONE"
                 (not (Option.isSome (G.Mat3.inverse singular)))

      val _ = Harness.section "Mat4"
      val () = checkMat4 "Mat4.mul id left"
                 (G.Mat4.translate (G.Vec3.v (1.0,2.0,3.0)),
                  G.Mat4.mul (G.Mat4.id, G.Mat4.translate (G.Vec3.v (1.0,2.0,3.0))))
      (* associativity over a fixed triple *)
      val t1 = G.Mat4.translate (G.Vec3.v (1.0, 0.0, 0.0))
      val r1 = G.Mat4.rotateZ (G.radians 90.0)
      val s1 = G.Mat4.scaleM (G.Vec3.v (2.0, 3.0, 4.0))
      val () = checkMat4 "Mat4.mul associativity"
                 (G.Mat4.mul (G.Mat4.mul (t1, r1), s1),
                  G.Mat4.mul (t1, G.Mat4.mul (r1, s1)))
      val () = checkMat4 "Mat4.transpose involution"
                 (r1, G.Mat4.transpose (G.Mat4.transpose r1))
      (* translate applies to a point *)
      val () = checkVec3 "Mat4.translate point"
                 (G.Vec3.v (3.0, 4.0, 5.0),
                  G.Mat4.transformPoint (G.Mat4.translate (G.Vec3.v (1.0,2.0,3.0)),
                                         G.Vec3.v (2.0,2.0,2.0)))
      (* translate does NOT affect a direction *)
      val () = checkVec3 "Mat4.translate dir unaffected"
                 (G.Vec3.v (2.0, 2.0, 2.0),
                  G.Mat4.transformDir (G.Mat4.translate (G.Vec3.v (1.0,2.0,3.0)),
                                       G.Vec3.v (2.0,2.0,2.0)))
      (* scale *)
      val () = checkVec3 "Mat4.scaleM point"
                 (G.Vec3.v (2.0, 6.0, 12.0),
                  G.Mat4.transformPoint (G.Mat4.scaleM (G.Vec3.v (2.0,3.0,4.0)),
                                         G.Vec3.v (1.0,2.0,3.0)))
      (* rotateZ 90 deg maps x -> y *)
      val () = checkVec3 "Mat4.rotateZ 90 maps x->y"
                 (G.Vec3.v (0.0, 1.0, 0.0),
                  G.Mat4.transformDir (G.Mat4.rotateZ (G.radians 90.0),
                                       G.Vec3.v (1.0, 0.0, 0.0)))
      (* rotateX/Y/Z agree with rotate about basis axes *)
      val () = checkMat4 "Mat4.rotateX = rotate about X"
                 (G.Mat4.rotate (0.7, G.Vec3.v (1.0,0.0,0.0)), G.Mat4.rotateX 0.7)
      val () = checkMat4 "Mat4.rotateY = rotate about Y"
                 (G.Mat4.rotate (0.7, G.Vec3.v (0.0,1.0,0.0)), G.Mat4.rotateY 0.7)
      val () = checkMat4 "Mat4.rotateZ = rotate about Z"
                 (G.Mat4.rotate (0.7, G.Vec3.v (0.0,0.0,1.0)), G.Mat4.rotateZ 0.7)
      (* inverse round-trip *)
      val mm = G.Mat4.mul (t1, G.Mat4.mul (r1, s1))
      val () = (case G.Mat4.inverse mm of
                  SOME inv => checkMat4 "Mat4.inverse: m*inv = id"
                                (G.Mat4.id, G.Mat4.mul (mm, inv))
                | NONE => Harness.check "Mat4.inverse: m*inv = id" false)
      (* edge: singular -> NONE *)
      val () = Harness.check "Mat4.inverse singular = NONE"
                 (not (Option.isSome (G.Mat4.inverse (G.Mat4.scaleM (G.Vec3.v (0.0,1.0,1.0))))))

      (* perspective: canonical GL entries *)
      val per = G.Mat4.perspective {fovy = G.radians 90.0, aspect = 1.0,
                                    near = 1.0, far = 101.0}
      val pl = G.Mat4.toList per
      (* column-major: index c*4+r. f = 1/tan(45)=1. [0]=f/aspect=1; [5]=f=1 *)
      val () = checkClose "perspective [0,0]=f/aspect" (1.0, List.nth (pl, 0))
      val () = checkClose "perspective [1,1]=f" (1.0, List.nth (pl, 5))
      (* m[2,2] = (far+near)/(near-far) = 102/-100 = -1.02 ; index col2 row2 = 10 *)
      val () = checkClose "perspective [2,2]" (~1.02, List.nth (pl, 10))
      (* m[3,2] = -1 ; col2 row3 = index 11 *)
      val () = checkClose "perspective [3,2] = -1" (~1.0, List.nth (pl, 11))
      (* m[2,3] = 2*far*near/(near-far) = 202/-100 = -2.02 ; col3 row2 = 14 *)
      val () = checkClose "perspective [2,3]" (~2.02, List.nth (pl, 14))

      (* ortho symmetric box maps corners to NDC *)
      val ort = G.Mat4.ortho {left = ~2.0, right = 2.0, bottom = ~2.0,
                              top = 2.0, near = ~2.0, far = 2.0}
      val () = checkVec3 "ortho maps right/top/far corner to (1,1,-1)?"
                 (G.Vec3.v (1.0, 1.0, ~1.0),
                  G.Mat4.transformPoint (ort, G.Vec3.v (2.0, 2.0, 2.0)))
      val () = checkVec3 "ortho maps center to origin"
                 (G.Vec3.v (0.0, 0.0, 0.0),
                  G.Mat4.transformPoint (ort, G.Vec3.v (0.0, 0.0, 0.0)))

      (* lookAt maps eye -> origin, and is orthonormal *)
      val la = G.Mat4.lookAt {eye = G.Vec3.v (0.0, 0.0, 5.0),
                              center = G.Vec3.v (0.0, 0.0, 0.0),
                              up = G.Vec3.v (0.0, 1.0, 0.0)}
      val () = checkVec3 "lookAt maps eye to origin"
                 (G.Vec3.v (0.0, 0.0, 0.0),
                  G.Mat4.transformPoint (la, G.Vec3.v (0.0, 0.0, 5.0)))
      (* the point in front of the eye should land on -Z *)
      val frontZ = G.Vec3.z (G.Mat4.transformPoint (la, G.Vec3.v (0.0, 0.0, 0.0)))
      val () = Harness.check "lookAt: target in front is at negative z"
                 (frontZ < 0.0)

      (* frustum: a symmetric frustum equals the matching perspective *)
      val _ = Harness.section "frustum / project"
      val fru = G.Mat4.frustum {left = ~1.0, right = 1.0, bottom = ~1.0,
                                top = 1.0, near = 1.0, far = 101.0}
      val () = checkMat4 "frustum symmetric = perspective(fovy=90)"
                 (G.Mat4.perspective {fovy = G.radians 90.0, aspect = 1.0,
                                      near = 1.0, far = 101.0},
                  fru)
      (* frustum: canonical GL entries (column-major index c*4+r) *)
      val fl = G.Mat4.toList fru
      val () = checkClose "frustum [0,0] = 2n/(r-l)" (1.0, List.nth (fl, 0))
      val () = checkClose "frustum [1,1] = 2n/(t-b)" (1.0, List.nth (fl, 5))
      val () = checkClose "frustum [2,2] = -(f+n)/(f-n)" (~1.02, List.nth (fl, 10))
      val () = checkClose "frustum [3,2] = -1" (~1.0, List.nth (fl, 11))
      val () = checkClose "frustum [2,3] = -2fn/(f-n)" (~2.02, List.nth (fl, 14))
      (* off-center frustum: x/y shear entries [0,2], [1,2] *)
      val fru2 = G.Mat4.frustum {left = 0.0, right = 4.0, bottom = 0.0,
                                 top = 2.0, near = 1.0, far = 5.0}
      val fl2 = G.Mat4.toList fru2
      val () = checkClose "frustum [0,2] = (r+l)/(r-l)" (1.0, List.nth (fl2, 8))
      val () = checkClose "frustum [1,2] = (t+b)/(t-b)" (1.0, List.nth (fl2, 9))

      (* project / unproject round-trip *)
      val proj = G.Mat4.perspective {fovy = G.radians 60.0, aspect = 4.0/3.0,
                                     near = 0.1, far = 100.0}
      val view = G.Mat4.lookAt {eye = G.Vec3.v (0.0, 0.0, 5.0),
                                center = G.Vec3.zero,
                                up = G.Vec3.v (0.0, 1.0, 0.0)}
      val vp = {x = 0.0, y = 0.0, width = 800.0, height = 600.0}
      val p = G.Vec3.v (0.5, ~0.3, 1.2)
      val win = G.project {obj = p, model = view, proj = proj, viewport = vp}
      val () = checkVec3 "unproject (project p) = p"
                 (p, G.unproject {win = win, model = view, proj = proj,
                                  viewport = vp})
      (* the view center (origin) lands at the middle of the viewport *)
      val ctr = G.project {obj = G.Vec3.zero, model = view, proj = proj,
                           viewport = vp}
      val () = checkClose "project origin -> screen center x"
                 (400.0, G.Vec3.x ctr)
      val () = checkClose "project origin -> screen center y"
                 (300.0, G.Vec3.y ctr)
    in
      ()
    end
end
