\begin{code}

module LinesPlanesOpen where

import AnGeo
import Lines
import LinesPlanes
\end{code}


Преобразование типов плоскостей:

\begin{code}
planeToCPlane :: Plane -> CPlane
planeToCPlane pl = CPl (vx (normal pl)) (vy (normal pl)) (vz (normal pl)) ((-1)*(sprod (mo pl) (normal pl)))

cplaneToPlane :: CPlane -> Plane
cplaneToPlane cpl = Pl (findVec cpl) (Vc (aa cpl) (bb cpl) (cc cpl))
      where findVec :: CPlane -> Vec 
            findVec cpl | ((aa cpl) == 0 && (bb cpl) == 0) = (Vc 0 0 (-(dd cpl) / (cc cpl)))
                        | ((aa cpl) == 0 && (cc cpl) == 0) = (Vc 0 (-(dd cpl) / (bb cpl)) 0)
                        | otherwise = (Vc (-(dd cpl) / (aa cpl)) 0 0)
\end{code}

Красивое отображение канонической плоскости в виде уравнения (Ax + By + Cz + D = 0):

\begin{code}
instance Show CPlane where
  show cplane = "("++(show (aa cplane))++")x + ("++(show (bb cplane))++")y + ("++(show (cc cplane))++")z + ("++(show (dd cplane))++") = 0"
\end{code}

отображение плоскости в каком-либо приемлемом виде :

\begin{code}
instance Show Plane where
  show plane = (show (vx (normal plane)))++"(x - " ++ (show (vx (mo plane))) ++ ") + "++(show (vy (normal plane)))++"(y - " ++ (show (vy (mo plane))) ++ ") + "++(show (vz (normal plane)))++"(z - " ++ (show (vz (mo plane))) ++ ") = 0"
\end{code}


Проверка принадлежности точки плоскости (в обеих формах)

\begin{code}
pointOnPlane :: Point -> Plane -> Bool
pointOnPlane pt plane | ((vx (normal plane)) * ((px pt) - (vx (mo plane))) + (vy (normal plane)) * ((py pt) - (vy (mo plane))) + (vz (normal plane)) * ((pz pt) - (vz (mo plane))) == 0) = True
                      | otherwise = False

pointOnCPlane :: Point -> CPlane -> Bool
pointOnCPlane pt cplane | (((aa cplane) * (px pt) + (bb cplane) * (py pt) + (cc cplane) * (pz pt) + (dd cplane)) == 0) = True
                        | otherwise = False
\end{code}

Проверка принадлежности прямой плоскости

\begin{code}
lineOnPlane :: Line -> Plane -> Bool
lineOnPlane ln plane | (((vx (normal plane)) * ((vx (ro ln)) - (vx (mo plane))) + (vy (normal plane)) * (vy (ro ln) - (vy (mo plane))) + (vz (normal plane)) * (vz (ro ln) - (vz (mo plane))) == 0) && (sprod (normal plane) (dir ln)) == 0) = True
                     | otherwise = False

lineOnCPlane :: Line -> CPlane -> Bool
lineOnCPlane ln cplane | ((((aa cplane) * (vx (ro ln)) + (bb cplane) * (vy (ro ln)) + (cc cplane) * (vz (ro ln)) + (dd cplane)) == 0) && (sprod (dir ln) (Vc (aa cplane) (bb cplane) (cc cplane)) == 0)) = True
                       | otherwise = False
\end{code}

Проверка совпадения двух плоскостей

\begin{code}
instance Eq Plane where
  pl1 == pl2 = planeToCPlane(pl1) == planeToCPlane(pl2)

equal :: (Eq a) => [a] -> Bool
equal xs = and $ map (== head xs) (tail xs)

instance Eq CPlane where
  cpl1 == cpl2 = equal [((aa cpl1) / (aa cpl2)), ((bb cpl1) / (bb cpl2)), ((cc cpl1) / (cc cpl2)), ((dd cpl1) / (dd cpl2))]
\end{code}

Проверка параллельности двух плоскостей

\begin{code}
planeParall :: Plane -> Plane -> Bool
planeParall pl1 pl2 = cplaneParall (planeToCPlane pl1) (planeToCPlane pl2)

cplaneParall :: CPlane -> CPlane -> Bool
cplaneParall cpl1 cpl2 | (equal [((aa cpl1) / (aa cpl2)), ((bb cpl1) / (bb cpl2)), ((cc cpl1) / (cc cpl2))] && not (equal [((aa cpl1) / (aa cpl2)), ((bb cpl1) / (bb cpl2)), ((cc cpl1) / (cc cpl2)), ((dd cpl1) / (dd cpl2))])) = True
                       | otherwise = False

\end{code}

Проверка перпендикулярности двух плоскостей

\begin{code}
planePerp :: Plane -> Plane -> Bool
planePerp p1 p2 = (normal p1) ┴ (normal p2)
\end{code}

\begin{code}
cplanePerp :: CPlane -> CPlane -> Bool
cplanePerp cpl1 cpl2 = (Vc (aa cpl1) (bb cpl1) (cc cpl1)) ┴ (Vc (aa cpl2) (bb cpl2) (cc cpl2))
\end{code}

Проверка параллельности прямой и плоскости

\begin{code}
lineAndPlaneParall :: Line -> Plane -> Bool
lineAndPlaneParall line plane = (dir line) ┴ (normal plane)
\end{code}

\begin{code}
lineAndCPlaneParall :: Line -> CPlane -> Bool
lineAndCPlaneParall line cpl = (dir line) ┴ (Vc (aa cpl) (bb cpl) (cc cpl))
\end{code}

Проверка перпендикулярности прямой и плоскости

\begin{code}
linePlanePerp :: Line -> Plane -> Bool
linePlanePerp line plane = equal [((vx (normal plane)) / (vx (dir line))), ((vy (normal plane)) / (vy (dir line))), ((vz (normal plane)) / (vz (dir line)))]

lineCPlanePerp :: Line -> CPlane -> Bool
lineCPlanePerp line cpl = equal [((aa cpl) / (vx (dir line))), ((bb cpl) / (vy (dir line))), ((cc cpl) / (vz (dir line)))] 
\end{code}

Нахождение угла между плоскостями (в градусах бы)...

\begin{code}
planeAngle  :: Plane -> Plane -> Double
planeAngle pl1 pl2 = acos (abs (sprod (normal pl1) (normal pl2)) / (sqrt ((vx (normal pl1)) ** 2 + (vy (normal pl1)) ** 2 + (vz (normal pl1)) ** 2) * (sqrt ((vx (normal pl2)) ** 2 + (vy (normal pl2)) ** 2 + (vz (normal pl2)) ** 2)))) * (180 / pi)

cplaneAngle :: CPlane -> CPlane  -> Double
cplaneAngle cpl1 cpl2 = acos (abs (sprod (Vc (aa cpl1) (bb cpl1) (cc cpl1)) (Vc (aa cpl2) (bb cpl2) (cc cpl2))) / (sqrt ((aa cpl1) ** 2 + (bb cpl1) ** 2 + (cc cpl1) ** 2) * (sqrt ((aa cpl2) ** 2 + (bb cpl2) ** 2 + (cc cpl2) ** 2)))) * (180 / pi)
\end{code} 

Нахождение угла между прямой и плоскостью (в градусах бы)...

\begin{code}
lineAndPlaneAngle :: Line -> Plane -> Double
lineAndPlaneAngle ln pl = asin ((abs (sprod (dir ln) (normal pl))) / (sqrt ((vx (normal pl)) ** 2 + (vy (normal pl)) ** 2 + (vz (normal pl)) ** 2) * sqrt ((vx (dir ln)) ** 2 + (vy (dir ln)) ** 2 + (vz (dir ln)) ** 2))) * (180 / pi)

lineAndCPlaneAngle :: Line -> CPlane -> Double
lineAndCPlaneAngle ln cpl = asin ((abs (sprod (dir ln) (Vc (aa cpl) (bb cpl) (cc cpl)))) / (sqrt ((aa cpl) ** 2 + (bb cpl) ** 2 + (cc cpl) ** 2) * sqrt ((vx (dir ln)) ** 2 + (vy (dir ln)) ** 2 + (vz (dir ln)) ** 2))) * (180 / pi)
\end{code}

Нахождение расстояния между точкой и плоскостью

\begin{code}
pointToPLaneDistance :: Point -> Plane -> Double
pointToPLaneDistance pt pl = pointToCPLaneDistance pt (planeToCPlane pl)

pointToCPLaneDistance :: Point -> CPlane -> Double
pointToCPLaneDistance pt cpl = abs ((aa cpl) * (px pt) + (bb cpl) * (py pt) + (cc cpl) * (pz pt) + (dd cpl)) / sqrt ((aa cpl) ** 2 + (bb cpl) ** 2 + (cc cpl) ** 2)
\end{code}

Нахождение линии пересечения двух плоскостей

\begin{code}
lineIntersectionOf2Planes :: Plane -> Plane -> Line
lineIntersectionOf2Planes pl1 pl2 = lineIntersectionOf2CPlanes (planeToCPlane pl1) (planeToCPlane pl2)

lineIntersectionOf2CPlanes :: CPlane -> CPlane -> Line
lineIntersectionOf2CPlanes cpl1 cpl2 | cplaneParall cpl1 cpl2 = error "Плоскости не пересекаются"
                                     | cpl1 == cpl2 = error "Плоскости совпадают"
                                     | otherwise = (Ln (Vc ((det (-(dd cpl1)) (bb cpl1) (-(dd cpl2)) (bb cpl2)) / (det (aa cpl1) (bb cpl1) (aa cpl2) (bb cpl2))) ((det (aa cpl1) (-(dd cpl1)) (aa cpl2) (-(dd cpl2))) / (det (aa cpl1) (bb cpl1) (aa cpl2) (bb cpl2))) 0) (vprod (Vc (aa cpl1) (bb cpl1) (cc cpl1)) (Vc (aa cpl2) (bb cpl2) (cc cpl2))))
      where det :: Double -> Double -> Double -> Double -> Double
            det a11 a12 a21 a22 = (a11 * a22) - (a12 * a21)
\end{code}
