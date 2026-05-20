import System.Random (randomRIO) -- SRC : reddit r/haskell

-- SRC : IA Gemini
import qualified Data.Map as Map
import Data.Map (Map)  

-- SRC : IA le chat / stackoverflow
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort (ViewPort)
import Graphics.Gloss.Interface.Pure.Game (play, Event(..), Key(..), KeyState(..), SpecialKey(..))
import Codec.Picture (readPng, convertRGBA8)
import Graphics.Gloss.Juicy (fromImageRGBA8)


-- =========================================================================
-- PARTIE 1 : PRELIMINAIRES
-- =========================================================================


-- somme des entiers d'une liste
somme :: [Integer] -> Integer
somme [] = 0
somme (h:t) = h + (somme t)


-- decide si un element valide le predicat
ilExiste :: (a -> Bool) -> [a] -> Bool
ilExiste f [] = False
ilExiste f (h:t) = (f h) || (ilExiste f t)


-- liste des entiers naturels decroissants depuis l'argument
jsqZer :: Integer -> [Integer]
jsqZer 0 = [0]
jsqZer i = i : (jsqZer (i-1))


-- liste des entiers naturels croissants jusqu'a l'argument
depZer :: Integer -> [Integer]
depZer 0 = [0]
depZer i = (depZer (i-1)) ++ [i] 


-- liste composées de 0
zeros :: Integer -> [Integer]
zeros 0 = []
zeros n = 0 : (zeros (n-1))


-- =========================================================================
-- PARTIE 2 : ENVIRONNEMENT
-- =========================================================================

data Ecran = Ecran { ecrH :: Integer, ecrL :: Integer }

data Coord = Coord { cx::Integer, cy::Integer } deriving Eq 

data Obstacle = Caillou Coord

data Joueuse = Joueuse { jCoord::Coord, jPV::Float, munition :: Int }

data Statut = Perdu | Gagne | EnCours deriving Eq

data Tir = Tir Coord  

data Bonus = BonusPV Coord | BonusMunition Coord 


data Envi = Envi {
    envEcr :: Ecran,
    envJou :: Joueuse,
    envObs :: [Obstacle],
    envTir :: [Tir],   -- liste de tirs
    envBon :: [Bonus], -- liste de bonus
    envSt  :: Statut
}


data Niveau = Facile | Intermediaire | Difficile deriving (Eq, Ord)

-- SRC : TME (sauf le TIR et BONUS)
data Case = TIR | CAILLOU | JOU | BONUSPV | BONUSMUN | VIDE deriving Eq

-- SRC : TME (sauf le TIR et BONUS)
instance Show Case where 
    show TIR = "*"
    show CAILLOU = "■" 
    show BONUSPV = "★"
    show BONUSMUN = "M"
    show JOU = "△"
    show VIDE = " "


-- =========================================================================
-- PARTIE 3 : MONAD
-- SRC : TME
-- =========================================================================

data Etat s a = Etat(s -> (s,a)) 

instance Functor (Etat s) where
    fmap f (Etat p1) = Etat (\x -> let (x', y) = p1 x in (x', f y))

instance Applicative (Etat s) where
    pure y = Etat (\x -> (x,y))            
    (<*>) (Etat pf) (Etat py) = Etat (\x -> let (x', f) = pf x 
                                                (x'', y) = py x' 
                                            in (x'', f y))

instance Monad (Etat s) where
    (>>=) (Etat p1) f = Etat (\x -> let (x', y) = p1 x 
                                        (Etat p2) = f y 
                                    in p2 x')
                
type EtatJeu = Etat Envi 


-- =========================================================================
-- PARTIE 4 : LOGIQUE DU JEU 
-- =========================================================================

data Direction = H | B | G | D | N deriving Eq 

-- Deplace purement la joueuse
depJo :: Joueuse -> Direction -> Joueuse
depJo (Joueuse (Coord x y) pv munition) H =  if y<9 
                                    then Joueuse (Coord x (y+1)) pv munition
                                    else Joueuse (Coord x y) pv munition
depJo (Joueuse (Coord x y) pv munition) B =  if y>0
                                    then Joueuse (Coord x (y-1)) pv munition
                                    else Joueuse (Coord x y) pv munition
depJo (Joueuse (Coord x y) pv munition) G =  if x>0 
                                    then Joueuse (Coord (x-1) y) pv munition
                                    else Joueuse (Coord x y) pv munition
depJo (Joueuse (Coord x y) pv munition) D =  if x<9 
                                    then Joueuse (Coord (x+1) y) pv munition
                                    else Joueuse (Coord x y) pv munition
depJo (Joueuse (Coord x y) pv munition) N = Joueuse (Coord x y) pv munition

-- Convertit touche clavier en action
actionDepJo :: Char -> Joueuse -> Joueuse
actionDepJo 'z' jo = depJo jo H
actionDepJo 'q' jo = depJo jo G
actionDepJo 's' jo = depJo jo B
actionDepJo 'd' jo = depJo jo D
actionDepJo _   jo = depJo jo N -- si on tape autre chose on bouge pas


-- tirer = ajouter un tir (juste en face de la joueuse) dans la liste des tirs de l'environnement
-- enleve une munition à la joueuse
tirer :: Envi -> Envi
tirer (Envi ecr (Joueuse (Coord x y) pv munition) obs tirs bonus st) = if munition > 0 -- si on a assez de munition on peut tirer
                                                                       then (Envi ecr (Joueuse (Coord x y) pv (munition-1)) obs ((Tir ( Coord x (y+1))):tirs) bonus st )
                                                                       else (Envi ecr (Joueuse (Coord x y) pv munition) obs tirs bonus st) -- sinon on renvoit l'envi
-- Met a jour l'environnement avec la touche de deplacement
actionDepEnv :: Envi -> Char -> Envi
actionDepEnv (Envi ecr jo obs tirs bonus st) c = Envi ecr (actionDepJo c jo) obs tirs bonus st

-- Dans la monade
action :: Char -> EtatJeu ()
action ' ' = Etat (\env -> (tirer env, ())) -- action de tirer
action c = Etat (\env -> (actionDepEnv env c, ())) -- action de deplacement


-- =========================================================================
-- PARTIE 5 : COLLISIONS, PERDU, SCROLL, AFFICHAGE ET TOUR
-- =========================================================================

-- SRC : TME 
toucheObs :: Coord -> Obstacle -> Bool 
toucheObs (Coord x y) (Caillou (Coord x' y')) = x' == x && y' == y 



toucheTir :: Coord -> Tir -> Bool 
toucheTir (Coord x y) (Tir (Coord xT yT)) = xT == x && yT == y

toucheBonusPV :: Coord -> Bonus -> Bool
toucheBonusPV (Coord x y) (BonusPV (Coord x' y')) = x==x' && y==y'
toucheBonusPV (Coord x y) _ = False

toucheBonusMuni :: Coord -> Bonus -> Bool
toucheBonusMuni (Coord x y) (BonusMunition (Coord x' y')) = x==x' && y==y'
toucheBonusMuni (Coord x y) _ = False


-- SRC : TME (sauf TIR et BONUS)
contenu :: Coord -> Envi -> Case
contenu co env | jCoord (envJou env) == co = JOU 
               | ilExiste (toucheTir co) (envTir env) = TIR
               | ilExiste (toucheObs co) (envObs env) = CAILLOU
               | ilExiste (toucheBonusPV co) (envBon env) = BONUSPV
               | ilExiste (toucheBonusMuni co) (envBon env) = BONUSMUN
               | otherwise = VIDE


-- L'affichage du terrain
-- SRC : TME 
instance Show Envi where 
    show env = (foldr (\y acc -> foldr (ligne env y) ("\n" <> acc) (depZer (ecrL (envEcr env))))
                        "\n"
                        (jsqZer (ecrH (envEcr env)))
                )
                <> "PV: " <> show (jPV (envJou env)) <> "\n"
        where ligne env y x acc = (show $ contenu (Coord x y) env) <> acc


-- Faire descendre les cailloux d'une case
-- SRC : TME 
descendreObs :: Obstacle -> Obstacle
descendreObs (Caillou (Coord x y)) = Caillou (Coord x (y-1))


-- faire descendre les bonus d'une case
descendreBonus :: Bonus -> Bonus 
descendreBonus (BonusPV (Coord x y)) = BonusPV (Coord x (y-1))
descendreBonus (BonusMunition (Coord x y)) = BonusMunition (Coord x (y-1))


-- Faire monter les tirs d'une case
monterTir :: Tir -> Tir
monterTir (Tir (Coord x y)) = Tir (Coord x (y+1))

-- SRC : TME 
scrollEnv :: Envi -> Envi 
scrollEnv (Envi ecr jo obs tirs bonus st) = Envi ecr jo (map descendreObs obs) tirs (map descendreBonus bonus) st


-- pour scroller que les tirs
scrollEnvTir :: Envi -> Envi 
scrollEnvTir (Envi ecr jo obs tirs bonus st) = Envi ecr jo obs (map monterTir tirs) bonus st

-- SRC : TME 
scroll :: EtatJeu ()
scroll = Etat (\env -> (scrollEnv env, ()))

scrollTir :: EtatJeu ()
scrollTir = Etat (\env -> (scrollEnvTir env , ()))

-- on regarde si un tir touche un Obstacle 
tirToucheObstacle :: Tir -> Obstacle -> Bool
tirToucheObstacle (Tir coordTir) (Caillou coordObs) = coordObs == coordTir

-- ON VERIFIE SI UN TIR TOUCHE UN OBSTACLE parmis les obstacles d'une liste 
tirToucheObstacles :: Tir -> [Obstacle] -> (Bool,[Obstacle])
tirToucheObstacles _ [] = (False,[])
tirToucheObstacles tir ( obs : t) = if tirToucheObstacle tir obs  
                                    then (True, t)
                                    else 
                                        let (b, o) = tirToucheObstacles tir t in
                                        (b, obs : o)


-- on regarde pour TOUS les tirs et TOUS les obstacles
toutTirToucheToutObstacle :: [Tir] -> [Obstacle] -> ([Tir], [Obstacle])
toutTirToucheToutObstacle [] obs = ([], obs)
toutTirToucheToutObstacle (tirActuel:resteTir) obs = let ( res, new_obs) = tirToucheObstacles tirActuel obs in 
                                    if res then toutTirToucheToutObstacle resteTir new_obs
                                    else -- correction : IA Gemini 
                                        let (new_resteTir, o) = toutTirToucheToutObstacle resteTir new_obs in 
                                            ( tirActuel : new_resteTir, new_obs )

checkToutTirToucheToutObstacle :: EtatJeu ()
checkToutTirToucheToutObstacle = Etat (\ (Envi ecr jo obs tirs bonus st) -> 
                                        let (new_tirs, new_obs) = toutTirToucheToutObstacle tirs obs in
                                            ((Envi ecr jo new_obs new_tirs bonus st), ()))



-- Verifier si le joueur touche un caillou ce tour-ci
-- SRC : TME 
checkCollision :: EtatJeu ()
checkCollision = Etat (\(Envi ecr (Joueuse c pv munition) obs tirs bonus st) -> 
    if ilExiste (toucheObs c) obs 
    then (Envi ecr (Joueuse c (pv-1) munition) obs tirs bonus st, ()) -- Perd 1 PV
    else (   (Envi ecr (Joueuse c pv munition) obs tirs bonus st)  , ()   )  )


-- on cherchele bonus situé aux coordonées donnée parmis la liste des bonus puis on renvoie la liste sans ce bonus
findBonus :: Coord -> [Bonus] -> [Bonus]
findBonus (Coord x y) [] = []
findBonus (Coord x y) ( (BonusPV (Coord x' y')) : resteBonus) = if x==x' && y==y' then resteBonus
                                                              else 
                                                                let res = findBonus (Coord x y) resteBonus in 
                                                                    ((BonusPV (Coord x' y')):res)
findBonus (Coord x y) ( (BonusMunition (Coord x' y')) : resteBonus) = if x==x' && y==y' then resteBonus
                                                              else 
                                                                let res = findBonus (Coord x y) resteBonus in 
                                                                    ((BonusMunition (Coord x' y')):res)

checkCollisionBonus :: EtatJeu ()
checkCollisionBonus = Etat (\ (Envi ecr (Joueuse c pv munition) obs tirs bon st)->
    if ilExiste (toucheBonusPV c) bon -- si la joueuse touche un bonusPV
    then 
        -- on trouve recupere la nouvelle liste de bonus (dans laquelle on a enlevé le bonus touché)
        let new_bon = findBonus c bon in 
            ((Envi ecr (Joueuse c (pv+0.5) munition) obs tirs new_bon st),())
    else
        if ilExiste (toucheBonusMuni c) bon -- si la joueuse touche un bonus munition
        then 
            let new_bon = findBonus c bon in 
            ((Envi ecr (Joueuse c pv (munition+1)) obs tirs new_bon st),())

        else
            ((Envi ecr (Joueuse c pv munition) obs tirs bon st),())
        )


-- fonction qui regarde si le tir est loin et donc si on doit le supprimer
suppTir ::  Tir -> Envi -> Bool
suppTir  (Tir (Coord x y)) (Envi (Ecran ecrH ecrL) jo obs tirs bon st) = y > (ecrH - 1)

-- supprimes les tirs trop loin de la liste
suppTirs ::  [Tir] -> Envi -> [Tir]
suppTirs  [] _ = []
suppTirs  (tir : reste) env = if suppTir tir env then suppTirs reste env 
                                      else 
                                        let res = suppTirs reste env in 
                                            (tir : res)
                                    
                                            
suppTirsEnv :: Envi -> Envi
suppTirsEnv (Envi ecr jo obs tirs bon st) = let new_tirs = suppTirs tirs (Envi ecr jo obs tirs bon st) in 
                                            (Envi ecr jo obs new_tirs bon st)

suppT :: EtatJeu ()
suppT = Etat (\env -> (suppTirsEnv env , ()))

-- fonction qui regarde si le bonus est en dehors de l'ecran (car on le fait scroll donc le y change à chaque tour)
suppBon :: Bonus -> Bool
suppBon (BonusPV (Coord x y)) = y < 0
suppBon (BonusMunition (Coord x y)) = y < 0

-- fonction qui supprime tous les bonus qui ont ete scrollé trop bas
suppBonus :: [Bonus] -> [Bonus]
suppBonus [] = []
suppBonus (b : reste) = if suppBon b -- si il est trop bas
                        then suppBonus reste -- on le supprime de la liste et on regarde dans le reste des bonus
                        else -- sinon 
                            let suite = suppBonus reste in 
                                (b : suite)

suppBonusEnv :: Envi -> Envi 
suppBonusEnv (Envi ecr jo obs tirs bon st) = let new_bonus = suppBonus bon in (Envi ecr jo obs tirs new_bonus st)

suppB :: EtatJeu ()
suppB = Etat (\env -> (suppBonusEnv env , ()))



-- fonction qui regarde si l'obstacle est en dehors de l'ecran (car on le fait scroll donc le y change à chaque tour)
suppObs :: Obstacle -> Bool
suppObs (Caillou (Coord x y)) = y < 0

-- fonction qui supprime tous les obstacles qui ont ete scrollé trop bas
suppObstacles :: [Obstacle] -> [Obstacle]
suppObstacles [] = []
suppObstacles (o : reste) = if suppObs o -- si il est trop bas
                        then suppObstacles reste -- on le supprime de la liste et on regarde dans le reste des obstacles
                        else -- sinon 
                            let suite = suppObstacles reste in 
                                (o : suite)

suppObstaclesEnv :: Envi -> Envi 
suppObstaclesEnv (Envi ecr jo obs tirs bon st) = let new_obs = suppObstacles obs in (Envi ecr jo new_obs tirs bon st)

suppO :: EtatJeu ()
suppO = Etat (\env -> (suppObstaclesEnv env , ()))

-- Verifier si on a perdu
-- SRC : TME 
checkPerdu :: EtatJeu ()
checkPerdu = Etat (\(Envi ecr (Joueuse c pv munition) obs tirs bonus st) ->  
    if pv <= 0 
    then (Envi ecr (Joueuse c pv munition) obs tirs bonus Perdu, ())
    else (  (Envi ecr (Joueuse c pv munition) obs tirs bonus st)  , ()  )  )


-- Affichage dans la monade
-- SRC : TME 
affiche :: EtatJeu String 
affiche = Etat (\env -> (env, show env))


-- ici, on sépare la focntion tour en 2 focntions pour Gloss pour différencier 2 cas
-- dans gloss le jeu tourne tout le temps, donc on ne peut pas avancer d'un tour que quand le joueur bouge
-- (la fonction tour prend un Char en paramètre donc il fallait bouger pour faire un tour)
-- SRC : IA Google Gemini
-- 1er cas : gère les actions du joueur (prend une touche du clavier)
actionClavier :: Char -> EtatJeu String
actionClavier c = do 
    action c  

    checkCollision
    checkCollisionBonus

    affiche

-- 2e cas : gère le temps qui passe automatiquement
moteurDuTemps :: EtatJeu String
moteurDuTemps = do

    scrollTir 
    checkToutTirToucheToutObstacle
    suppT
    suppB
    suppO

    -- scroll en 2 fois pour eviter les bugs de tirs qui n'eliminent pas les obstacles

    scroll
    checkToutTirToucheToutObstacle
    checkCollisionBonus
    checkCollision
    
    checkPerdu
    checkGagne
    affiche


{- ANCIENNE FONCTION TOUR 
    -- SRC : TME 
    tour :: Char -> EtatJeu String
    tour c = do 
        action c  
        scrollTir -- scroll en 2 étapes pour éviter que un tir passe au dessus d'un obstacle
        checkToutTirToucheToutObstacle
        scroll
        checkCollision
        checkPerdu
        checkGagne
        affiche
-}


-- =========================================================================
-- PARTIE 6 : GAGNE
-- =========================================================================


-- fonction qui renvoie l'obstacle le plus loin
obsPlusLoin :: Obstacle -> [Obstacle] -> Obstacle
obsPlusLoin obsMax [] = obsMax
obsPlusLoin (Caillou (Coord maxX maxY)) ((Caillou (Coord x y)):t) = if y>maxY then obsPlusLoin (Caillou (Coord x y)) t else obsPlusLoin (Caillou (Coord maxX maxY)) t

obsEnviPlusLoin :: Envi -> Obstacle
obsEnviPlusLoin (Envi (Ecran ecrH ecrL) jo obs tirs bonus st) = obsPlusLoin (Caillou (Coord 0 (-100))) obs



-- fonction qui regarde si le joueur a gagné : si il a depassé tous les obstacles
{- 
   SRC (Aide) : IA Gemini
   Pour m'aider à débugger l'erreur de scope sur 'x' et 'y' ici, 
   et pour comprendre comment extraire proprement les coordonnées avec un 'let ... in'.
-}
checkGagne :: EtatJeu ()
checkGagne = Etat (\env@(Envi ecr joueuse@(Joueuse (Coord _ y) pv munition) obs tirs bonus st) -> 
    case obs of   -- permet de regarder directement l'état de la lsite obs
        [] -> (Envi ecr joueuse obs tirs bonus Gagne, ()) -- si plus d'obstacles : Gagne
        _  -> 
            -- si il reste des obstacles, on regarde le plus haut
            let (Caillou (Coord _ maxY)) = obsEnviPlusLoin env 
            in if y > maxY -- si la joueuse a depassé le dernier obstacle 
               then (Envi ecr joueuse obs tirs bonus Gagne, ()) -- alors elle a Gagne
               else (env, ()) -- sinon elle n'a pas finis donc on continue
    )


-- =========================================================================
-- PARTIE 6.5 : ADAPTATION GLOSS
-- =========================================================================

-- SRC : IA le chat /stackoverflow / reddit
-- Charge une image PNG (utilise JuicyPixels pour les PNG)
loadPNG :: FilePath -> IO Picture
loadPNG path = do
    eitherImg <- readPng path   --renvoie un Either String DyanmicImage
    case eitherImg of
        Left err -> error $ "Erreur: " ++ err
        Right dynImg -> let img = convertRGBA8 dynImg
                        in return (fromImageRGBA8 img)

--on adapte la taille des cases à la taille de l'image
caseSize :: Float
caseSize = 64    --nombre de pixels

-- SRC (Aide) : IA le chat
--Converti les Coord pour l'affichage par gloss où le 0,0 est au centre
toScreen :: Ecran -> Coord -> (Float, Float)
toScreen (Ecran h w) (Coord x y) =
    ( fromIntegral (x - w `div` 2) * caseSize +32  --les coordonnée gloss sont en float (fromIntegral)
    , fromIntegral (y - h `div` 2) * caseSize +32
    )


-- SRC (Aide) : IA le chat
--Dessine tout l'environnement du jeu
renderEnv :: Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Ecran -> Envi -> Picture
renderEnv bgImg playerImg obsImg tirImg lifeImg winImg loseImg shellImg(Ecran h w) env@(Envi _ (Joueuse playerCoord pv munition) obs tirs bonus st) =  
    pictures $      
        [ translate 0 0 bgImg ] ++      
        [ translate ((fst pos)) ((snd pos)) obsImg | Caillou c <- obs, let pos = toScreen (Ecran h w) c , (snd pos)<320 && (snd pos)>(-320)] ++  
        [ translate ((fst pos)) ((snd pos)) tirImg | Tir coordTir <- tirs, let pos = toScreen (Ecran h w) coordTir, (snd pos)<320 && (snd pos)>(-320)] ++ 
        [ translate ((fst pos)) ((snd pos)) lifeImg | BonusPV coordBonus <- bonus, let pos = toScreen (Ecran h w) coordBonus , (snd pos)<320 && (snd pos)>(-320)] ++ 
        
        [ translate ((fst pos)) ((snd pos))  shellImg | BonusMunition coordBonus <- bonus, let pos = toScreen (Ecran h w) coordBonus , (snd pos)<320 && (snd pos)>(-320)] ++ 

        [ translate ((fst pos)) ((snd pos)) playerImg | let pos = toScreen (Ecran h w) playerCoord ] ++   

        --affichage PV
        [ translate (-263) 306 $                    
            (color white $
            (rectangleSolid 80 20))
        ] ++ 
        [ translate (-263) 306 $                    
            (color orange $
            (rectangleWire 81 21))
        ] ++    
        [ translate (-300) 298 $                    
           (scale 0.15 0.15 $
            color orange $ 
            text ("PV: " ++ show pv))
        ] ++
        [ translate (-299) 298 $                    
           (scale 0.15 0.15 $
            color orange $ 
            text ("PV: " ++ show pv))
        ] ++
        --affichage AMO
        [ translate (263) 306 $                    
            (color white $
            (rectangleSolid 80 20))
        ] ++ 
        [ translate (263) 306 $                    
            (color (light magenta) $
            (rectangleWire 81 21))
        ] ++    
        [ translate (228) 298 $                    
           (scale 0.15 0.15 $
            color (light magenta) $ 
            text ("AMO " ++ show munition))
        ] ++
        [ translate (229) 298 $                    
           (scale 0.15 0.15 $
            color (light magenta) $ 
            text ("AMO " ++ show munition))
        ] ++

        case st of
            Perdu -> [  translate 0 0 loseImg ]
            Gagne -> [  translate 0 0 winImg ]
            _     -> []

-- SRC : IA le chat
-- Gère les entrées clavier (on délègue à la monade actionClavier)
handleInput :: Event -> Envi -> Envi
handleInput (EventKey (Char c) Down _ _) env = 
    let Etat p = actionClavier c
        (env', _) = p env 
    in env'

-- AJOUT POUR GERER LA TOUCHE ESPACE (TIR) AVEC GLOSS
handleInput (EventKey (SpecialKey KeySpace) Down _ _) env = 
    let Etat p = actionClavier ' ' 
        (env', _) = p env 
    in env'
handleInput _ env = env


-- SRC : IA le chat
-- Met à jour l'environnement (est appelé automatiquement par gloss)
updateEnv :: Float -> Envi -> Envi      
updateEnv _ env@(Envi _ _ _ _ _ EnCours) =                       
    let Etat p = moteurDuTemps 
        (env', _) = p env
    in env'

updateEnv _ env = env


-- =========================================================================
-- PARTIE IMPURE (IO) ET MAIN
-- =========================================================================

-- renvoie un nombre au hasard entre a et b
-- SRC : reddit r/haskell
randomInt :: Int -> Int -> IO Int           
randomInt a b = randomRIO (a, b)



-- Fonction qui renvoie une liste de n obstacle placés au hasard
{- 
    SRC (Aide) : IA Gemini 
    -- comme on est dans Impur (IO) on doit utiliser return et do etc
    -- le <- seulement pour les trucs impurs (ça ouvre les boites IO) et let pour les trucs purs
-}
listeObstacle :: Map (Integer,Integer) Bool -> Int -> Int -> Envi -> IO ( [Obstacle], Map (Integer,Integer) Bool)
listeObstacle dicoObs 0 _ _ = return ([], dicoObs)
listeObstacle dicoObs nbObs coeffH (Envi (Ecran ecrH ecrL) jo obs tirs bonus st) = do
    

    -- on doit avoir plus de chance d'avoir des obstacles vers le milieu de l'ecran sinon trop facile
    let chance = 60
    tirage <- (randomInt 1 100)

    -- on veut avoir 60% de chance d'être au centre de l'ecran
    x <- if tirage <= chance
        then 
            -- si on est dans les 60
            --  on sait que la largeur de l'ecran est 10, donc on prend un x entre 3 et 7
            randomInt 3 7

    else 
        -- sinon on place au hasard dans toute la largeur
        -- fromIntegral prend n'importe quel type de nb entier et le tranforme dans le type de nb que la fonction attend
        randomInt 0  ((fromIntegral ecrL)-1)  
    
    
    y <- (randomInt 5  (coeffH * (fromIntegral ecrH)))

    let coordX = (toInteger x) -- toInteger prend un Int et renvoie un Integer
    let coordY = (toInteger y) -- toInteger prend un Int et renvoie un Integer

    case Map.lookup (coordX,coordY) dicoObs of 
        Just _ -> 
            -- si les coordonées ont deja ete utilisé on rappelle la fonction
            listeObstacle dicoObs nbObs coeffH (Envi (Ecran ecrH ecrL) jo obs tirs bonus st)
        Nothing -> do 
            -- si on trouve pas de coordonnées alors on peut garder cet obsatcle
            let new_caillou =  Caillou ( Coord  coordX coordY )  
            let new_dicoObs = Map.insert (coordX,coordY) True dicoObs
            (suite,d) <- listeObstacle new_dicoObs (nbObs-1) coeffH (Envi (Ecran ecrH ecrL) jo obs tirs bonus st)
            
            -- on renvoie le dico avec les coord des obstacles pour le reutiliser ensuite 
            -- pour placer les bonus
            return ((new_caillou : suite ), d) 



-- SRC (Aide) : IA Gemini 
-- on peut prendre en paramètre un constructeur, en Haskell c'est comme une fonction 
-- BonusPV :: Coord -> Bonus


-- fonction qui, à partir d'un constructeur de bonus, fabrique une lsite de nbBonus bonus
genererListeBonus :: (Coord -> Bonus) -> Map (Integer,Integer) Bool -> Int -> Int -> Envi -> IO ([Bonus], Map (Integer,Integer) Bool)
genererListeBonus constructeur d 0 _ _ = return ([],d)
genererListeBonus constructeur dicoBonus nbBonus coeffH (Envi (Ecran ecrH ecrL) jo obs tirs bon st) = do 

    x <- (randomInt 0  ((fromIntegral ecrL)-1))
    y <- (randomInt 2  (coeffH * (fromIntegral ecrH)))

    let coordX = (toInteger x) -- toInteger prend un Int et renvoie un Integer
    let coordY = (toInteger y) -- toInteger prend un Int et renvoie un Integer


    case Map.lookup (coordX,coordY) dicoBonus of 
        Just _ -> 
            -- les coordonnées sont deja prises
            genererListeBonus constructeur dicoBonus nbBonus coeffH (Envi (Ecran ecrH ecrL) jo obs tirs bon st)
        Nothing -> do 
            -- les coordonées sont libres
            let new_bon =  constructeur ( Coord  coordX coordY )
            let new_dicoBonus = Map.insert (coordX,coordY) True dicoBonus
            (suite,d) <- genererListeBonus constructeur new_dicoBonus (nbBonus-1) coeffH (Envi (Ecran ecrH ecrL) jo obs tirs bon st)
            
            return ((new_bon : suite ), d)



listeBonus :: Map (Integer,Integer) Bool -> Int -> Int -> Envi -> IO [Bonus]
listeBonus dicoBonus nbBonus coeffH (Envi (Ecran ecrH ecrL) jo obs tirs bon st) = do 

    (listeBonusPV,dicoApres) <- genererListeBonus BonusPV dicoBonus nbBonus coeffH (Envi (Ecran ecrH ecrL) jo obs tirs bon st)
    (listeBonusMunition, _) <- genererListeBonus BonusMunition dicoApres nbBonus coeffH (Envi (Ecran ecrH ecrL) jo obs tirs bon st)

    
    let listeBonus = (listeBonusPV ++ listeBonusMunition)

    return listeBonus




-- la clé dans un map doit savoir être trié ! donc j'ai mis les niveau deriving (Eq,Ord)
-- SRC (Aide) : hackage.haskell.org
configNiveau :: Map Niveau (Int,Int,Int,Int)
configNiveau = Map.fromList [
    (Facile, (30, 5, 3, 5)),           --  Facile : 30 obstacles, 5x la hauteur, 3 bonusPV et 3 BonusMunition, 5 munitions
    (Intermediaire, (90, 10, 4, 4)),   --  Intermédiaire : 60 obstacles, 10x la hauteur, 4 bonusPV et 4 BonusMunition, 4 munitions
    (Difficile, (170, 20, 5, 3))      --  Difficile : 170 obstacles, 20x la hauteur, 5 bonusPV et 5 BonusMunition, 3 munitions
    ]



-- SRC (Aide) : IA Gemini
setPartie :: Int -> Int -> Int -> Int -> IO Envi -- IO car utilise une fonction IO (listeObstacle)
setPartie nbObs coeffH nbBonus nbMunitions = do 
    
    -- On fabrique notre partie de depart (Ecran 10x10, Joueuse en (5,0) avec 3 PV)
    let partieVide = Envi (Ecran 10 10) (Joueuse (Coord 5 0) 3 nbMunitions) [] [] [] EnCours 
    let dicoVide = Map.empty -- SRC : hackage.haskell.org
    (liste_obs,dico) <- listeObstacle dicoVide nbObs coeffH partieVide -- on construit la liste des obstacles (placés au hasard) en fonction de la difficulté choisit
    
    liste_bonus <- listeBonus dico nbBonus coeffH partieVide

    let partieInitiale = Envi (Ecran 10 10) (Joueuse (Coord 5 0) 3 nbMunitions) liste_obs [] liste_bonus EnCours

    return partieInitiale
    

{- ANCIENNES FONCTIONS INUTILES AVEC GLOSS 
-- Fait tourner la monade pour avoir le nouvel environnement
-- SRC : TME
faitTour :: Char -> Envi -> IO Envi
faitTour c env = do
    let Etat p = tour c
        (env', str) = p env
    putStrLn str
    return env'

-- La boucle de jeu infinie (s'arrete si on perd ou gagne)
-- SRC : TME 
boucle :: Envi -> IO ()
boucle env = case envSt env of
    Perdu -> putStrLn "====== GAME OVER ! TU AS PERDU !  ======"
    Gagne -> putStrLn "====== GAGNE ! TU ES TROP FORTE ! ======"
    _     -> do 
        putStrLn "Appuie sur z, q, s, ou d pour te déplacer, espace pour tirer, puis Entree:"
        c <- getChar
        _ <- getLine -- Pour absorber la touche Entree 
        env' <- faitTour c env
        boucle env'
-}

-- traduit le niveau choisit par le joueur sur l'entrée
lireNiveau :: Char -> Niveau 
lireNiveau '1' =  Facile
lireNiveau '2' =  Intermediaire
lireNiveau '3' =  Difficile
lireNiveau _   =  Facile


-- le main de la partie 
-- SRC : IA Gemini 
main :: IO ()
main = do
    putStrLn "=== BIENVENUE DANS PEW PEW ==="
    putStrLn ""       
    putStrLn "=== CHOISSISSEZ VOTRE NIVEAU ==="
    putStrLn ""
    putStrLn "===     Tapez 1 pour FACILE         ==="
    putStrLn "===     Tapez 2 pour INTERMEDIAIRE  ==="
    putStrLn "===     Tapez 3 pour DIFFICILE      ==="

    choix_niveau <- getChar -- on recupere le choix de niveau du joueur
    _ <- getLine 
    let level = lireNiveau choix_niveau -- on a le Niveau choisit


    -- on recupere les configurations associées à ce niveau dans notre Map        
    -- SRC (Aide) : hackage.haskell.org
    let (nbObs, coeffH, nbBonus, nbMunitions) = case Map.lookup level configNiveau of -- on cherche dans configNiveau les paramètres du niveau
                                                    Just bonne_valeurs -> bonne_valeurs -- si on trouve le niveau
                                                    Nothing -> (30, 5, 5,5)   -- si on ne trouve pas le niveau (ex : si le joueur entre 5) on met par defaut le niveau facile
                                

    partieInitiale <- setPartie nbObs coeffH nbBonus nbMunitions  -- SRC : IA Gemini : pour ouvrir la boite IO on utilise <- car setPartie renvoie un IO Envi
    

    putStrLn "Chargement des graphismes..."
    bgImg <- loadPNG "img/background.png"
    playerImg <- loadPNG "img/player.png"
    obsImg <- loadPNG "img/obs.png"
    tirImg <- loadPNG "img/projectile.png"
    lifeImg <- loadPNG "img/star.png"
    winImg <- loadPNG  "img/win.png"
    loseImg <- loadPNG  "img/gameover.png"
    shellImg <- loadPNG "img/shell.png"
    
    let ecr = Ecran 10 10

    putStrLn "Lancement du jeu graphique !"
    
    play
        (InWindow "Pew Pew" (650,650) (260, 50))  -- Fenêtre de 650x650 pixels
        black                                       -- Fond noir
        5                                           -- 5 FPS pour que le jeu soit fluide
        partieInitiale                              -- État initial
        (renderEnv bgImg playerImg obsImg tirImg lifeImg winImg loseImg shellImg ecr)      -- Fonction de rendu
        handleInput                                 -- Gestion des entrées
        updateEnv                                   -- Mise à jour automatique

