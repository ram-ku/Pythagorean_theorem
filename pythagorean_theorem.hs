{-# LANGUAGE FlexibleInstances,InstanceSigs,UndecidableInstances#-}
module Active_Canvas_Monoid where

import Graphics.Blank
import Animator
import Data.List
import Data.Active
import Data.Semigroup
import Control.Monad

instance Semigroup (Canvas ()) where
	(<>) = mappend

instance Monoid a => Monoid (Canvas a) where 
	mempty = return mempty
	mappend = liftM2 mappend

join_actives :: [Active (Canvas ())] -> Active (Canvas ())
join_actives xs = foldl1 (->>) xs
					
fromTimeF::Time -> Float
fromTimeF=fromTime

newActive1=mkActive (toTime 0) (toTime 10) (\x -> if (fromTimeF x) < 10 
													then triangle ((fromTimeF x)*20+400) (400-(fromTimeF x)*20) "down" "Yellow" "Black"
													else if ((fromTimeF x) > 10 && (fromTimeF x) < 38 ) 
														then triangle 600 200 "down" "Yellow" "Black"
														else triangle 600 200 "down" "rgba(255, 255, 0, 0.5)" "rgba(0, 0, 0, 0.5)")
newActive2=mkActive (toTime 0) (toTime 5) (\x ->if ((fromTimeF x) >= 0 && (fromTimeF x) < 38) 
													then triangle 500 350 "right" "Yellow" "Black"
													else if (fromTimeF x) > 38
														then triangle 500 350 "right" "rgba(255, 255, 0, 0.5)" "rgba(0, 0, 0, 0.5)"
														else return ())
newActive3=mkActive (toTime 0) (toTime 5) (\x ->if (fromTimeF x) >= 0 then renderTri3 else return ())
newActive4=mkActive (toTime 0) (toTime 5) (\x ->if (fromTimeF x) >= 0 then renderTri4 else return ())
newActive4a=mkActive (toTime 0) (toTime 5) (\x ->if ((fromTimeF x) >= 0 && (fromTimeF x) <=45) 
												then rectangle 601 300 48 49 "Yellow" "Black" 
												else if ((fromTimeF x) > 50)
													then rectangle 601 300 48 49 "rgba(255, 255, 0, 0.5)" "rgba(0, 0, 0, 0.5)"
													else return ())
newActiveHead=mkActive (toTime 0) (toTime 2) (\x ->if ((fromTimeF x) >= 0 ) then renderHead else return())
newActiveLabel=mkActive (toTime 0) (toTime 5) (\x ->if ((fromTimeF x) >= 0 && (fromTimeF x) < 5) 
													then renderLabel
													else return ())
newActiveLeftSlide=mkActive (toTime 0) (toTime 10) (\x -> if ((fromTimeF x) >= 0 && (fromTimeF x) <= 10) 
														then triangle (600-(fromTimeF x)*10) (200+(fromTimeF x)*15) "down" "Yellow" "Black"
														else if (fromTimeF x) > 10 
															 then triangle 500 350 "down" "Yellow" "Black";
															 else return ())
newActiveRightSlide=mkActive (toTime 0) (toTime 10) (\x -> if ((fromTimeF x) >= 0 && (fromTimeF x) <= 10) 
														then triangle (500+(fromTimeF x)*15) (350+(fromTimeF x)*10) "right" "Yellow" "Black"
														else if (fromTimeF x) > 10 
															 then triangle 650 450 "right" "Yellow" "Black";
															 else return ())
															 
newActiveSquares=mkActive (toTime 0) (toTime 3) (\x -> if (fromTimeF x) >= 0
														then do{ rectangle 500 350 100 100 "rgba(255, 255, 0, 0.8)" "Black";
																 rectangle 600 300 150 150 "rgba(255, 255, 0, 0.8)" "Black"}
														else return())
														
newActivefinalLabel=mkActive (toTime 0) (toTime 2) (\x ->if ((fromTimeF x) >= 0 ) then renderFinalLabel else return())


main :: IO ()
main = blankCanvas 3000 $ \ context -> play context $ simulate (toRational 30) $ join_actives [newActive1,newActive2,newActive3,newActive4,newActive4a,newActiveHead,newActiveLabel,newActiveLeftSlide,newActiveRightSlide,newActiveSquares,newActivefinalLabel]

play context (n:ns) = do{
				send context $ do {
					render n};
				play context ns}
play context [] = do { send context $ do {fillText ("The End",610,250)}}

renderTri3 ::Canvas ()
renderTri3 = triangle 650 450 "up" "Yellow" "Black"

renderTri4 ::Canvas ()
renderTri4 = triangle 750 300 "left" "Yellow" "Black";

renderHead :: Canvas ()
renderHead = do { 
				font "20px Verdana";
				fillStyle "Black";
				fillText ("area = c2",400,200);
				}
renderLabel ::Canvas ()
renderLabel = do { 
				font "20px Verdana";
				fillStyle "Black";
				fillText ("a",610,250);
				fillText ("b",655,290);
				fillText ("c", 680,250);
				fillText ("c", 690,410);
				}

renderFinalLabel::Canvas ()
renderFinalLabel = do {
				font "20px Verdana";
				fillStyle "Black";
				fillText ("a",480,400);
				fillText ("a",550,470);
				fillText ("b",675,470);
				fillText ("b",755,375);
				fillText ("c2=a2+b2",675,200);
				}