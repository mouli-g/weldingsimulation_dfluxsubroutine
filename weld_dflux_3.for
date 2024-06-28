      SUBROUTINE DFLUX(FLUX, SOL, KSTEP, KINC, TIME, NOEL, NPT, COORDS,
     &            JLTYP, TEMP, PRESS, SNAME)
C
      INCLUDE 'ABA_PARAM.INC'
      DIMENSION FLUX(2), TIME(2), COORDS(3)
      CHARACTER*80 SNAME
C
      X = COORDS(1)
      Y = COORDS(2)
      Z = COORDS(3)

      Xstart = 0.0
      Ystart = 0.0
      Zstart = 0.0104

      Xend = 0.1
      Yend = 0.0

      TotalTime = 10.0

      vx = (Xend - Xstart) / TotalTime
      vy = 0.0
      vz = 0.0

      IF (TIME(2) > TotalTime) THEN
          Xcentre = Xend
      ELSE
          Xcentre = Xstart + vx * TIME(2)
      END IF
          Ycentre = Ystart
          Zcentre = Zstart

      XT = Xcentre - X
      YT = Ycentre - Y
      Dist = SQRT((YT*YT) + (XT*XT))

      voltage = 11
      current = 80
      etaeff = 0.48
      rh = 0.005

      FLUX(1) = ((etaeff * voltage * current) / (3.141592654 * rh**2))* EXP(-(Dist**2) / (rh**2))
      FLUX(2) = 0.0

      RETURN
      END
