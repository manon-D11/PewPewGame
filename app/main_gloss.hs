import System.Random (randomRIO)
import System.IO (hSetBuffering, BufferMode(NoBuffering), stdin)
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort (ViewPort)
import Graphics.Gloss.Interface.Pure.Game (play, Event(..), Key(..), KeyState(..))
-- pour load les png
import Codec.Picture (readPng, convertRGBA8)
import Graphics.Gloss (Picture)
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

data Joueuse = Joueuse { jCoord::Coord, jPV::Integer }

data Statut = Perdu | Gagne | EnCours deriving Eq

data Envi = Envi {
    envEcr :: Ecran,
    envJou :: Joueuse,
    envObs :: [Obstacle],
    envSt  :: Statut
}

data Case = OBS | JOU | VIDE deriving Eq

instance Show Case where 
    show OBS = "O" 
    show JOU = "A"
    show VIDE = " "


-- =========================================================================
-- PARTIE 3 : LA MONADE D'ETAT
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
depJo (Joueuse (Coord x y) pv) H = Joueuse (Coord x (y+1)) pv
depJo (Joueuse (Coord x y) pv) B = Joueuse (Coord x (y-1)) pv
depJo (Joueuse (Coord x y) pv) G = Joueuse (Coord (x-1) y) pv
depJo (Joueuse (Coord x y) pv) D = Joueuse (Coord (x+1) y) pv
depJo (Joueuse (Coord x y) pv) N = Joueuse (Coord x y) pv

-- Convertit touche clavier en action
actionJo :: Char -> Joueuse -> Joueuse
actionJo 'z' jo = depJo jo H
actionJo 'q' jo = depJo jo G
actionJo 's' jo = depJo jo B
actionJo 'd' jo = depJo jo D
actionJo _   jo = depJo jo N -- si on tape autre chose on bouge pas

-- Met a jour l'environnement avec la touche
actionEnv :: Envi -> Char -> Envi
actionEnv (Envi ecr jo obs st) c = Envi ecr (actionJo c jo) obs st

-- Dans la monade
action :: Char -> EtatJeu ()
action c = Etat (\env -> (actionEnv env c, ()))


-- =========================================================================
-- PARTIE 5 : COLLISIONS ET AFFICHAGE
-- =========================================================================

toucheObs :: Coord -> Obstacle -> Bool 
toucheObs (Coord x y) (Caillou (Coord x' y')) = x' == x && y' == y 

contenu :: Coord -> Envi -> Case
contenu co env | jCoord (envJou env) == co = JOU 
               | ilExiste (toucheObs co) (envObs env) = OBS
               | otherwise = VIDE

-- L'affichage du terrain
instance Show Envi where 
    show env = (foldr (\y acc -> foldr (ligne env y) ("\n" <> acc) (depZer (ecrL (envEcr env))))
                        "\n"
                        (jsqZer (ecrH (envEcr env)))
                )
                <> "PV: " <> show (jPV (envJou env)) <> "\n"
        where ligne env y x acc = (show $ contenu (Coord x y) env) <> acc


-- Faire descendre les cailloux d'une case
descendreUN :: Obstacle -> Obstacle
descendreUN (Caillou (Coord x y)) = Caillou (Coord x (y-1))

scrollEnv :: Envi -> Envi 
scrollEnv (Envi ecr jo obs st) = Envi ecr jo (map descendreUN obs) st

scroll :: EtatJeu ()
scroll = Etat (\env -> (scrollEnv env, ()))


-- Verifier si le joueur touche un caillou ce tour-ci
checkCollision :: EtatJeu ()
checkCollision = Etat (\(Envi ecr (Joueuse c pv) obs st) -> 
    if ilExiste (toucheObs c) obs 
    then (Envi ecr (Joueuse c (pv-1)) obs st, ()) -- Perd 1 PV
    else (   (Envi ecr (Joueuse c pv) obs st)  , ()   )  )

-- Verifier si on a perdu
checkPerdu :: EtatJeu ()
checkPerdu = Etat (\(Envi ecr (Joueuse c pv) obs st) ->  
    if pv <= 0 
    then (Envi ecr (Joueuse c pv) obs Perdu, ())
    else (  (Envi ecr (Joueuse c pv) obs st)  , ()  )  )


-- Affichage dans la monade
affiche :: EtatJeu String 
affiche = Etat (\env -> (env, show env))

-- LE TOUR COMPLET DU JEU : prend une commande clabier et effectue un tour de jeu
tour :: Char -> EtatJeu String
tour c = do 
    action c  
    scroll
    checkCollision
    checkPerdu
    checkGagne
    affiche





-- fonction qui renvoie l'obstacle le plus loin
obsPlusLoin :: Obstacle -> [Obstacle] -> Obstacle
obsPlusLoin obsMax [] = obsMax
obsPlusLoin (Caillou (Coord maxX maxY)) ((Caillou (Coord x y)):t) = if y>maxY then obsPlusLoin (Caillou (Coord x y)) t else obsPlusLoin (Caillou (Coord maxX maxY)) t

obsEnviPlusLoin :: Envi -> Obstacle
obsEnviPlusLoin (Envi (Ecran ecrH ecrL) jo obs st) = obsPlusLoin (Caillou (Coord 0 0)) obs


-- fonction qui regarde si le joueur a gagné : si il a depassé tous les obstacles
checkGagne :: EtatJeu ()
checkGagne = Etat (\env@(Envi ecr joueuse@(Joueuse (Coord _ y) pv) obs st) -> 
    case obs of
        [] -> (Envi ecr joueuse obs Gagne, ()) -- si plus d'obstacles : Gagne
        _  -> 
            -- si il reste des obstacles, on regarde le plus haut
            let (Caillou (Coord _ maxY)) = obsEnviPlusLoin env 
            in if y > maxY 
               then (Envi ecr joueuse obs Gagne, ())
               else (env, ())
    )

-- =========================================================================
-- PARTIE 6 : ADAPTATION GLOSS
-- =========================================================================


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
caseSize = 50     --nombre de pixels


--Converti les Coord pour l'affichage par gloss où le 0,0 est au centre
toScreen :: Ecran -> Coord -> (Float, Float)
toScreen (Ecran h w) (Coord x y) =
    ( fromIntegral (x - w `div` 2) * caseSize --les coordonnée gloss sont en float (fromIntegral)
    , fromIntegral (y - h `div` 2) * caseSize
    )


--Dessine tout l'environnement du jeu
renderEnv :: Picture -> Picture -> Picture -> Ecran -> Envi -> Picture
renderEnv bgImg playerImg obsImg (Ecran h w) env@(Envi _ (Joueuse playerCoord pv) obs st) =  -- env@ on garde le nom de la var env en la décomposant 
    pictures $      -- fonction qui combine les images
        [ translate 0 0 bgImg ] ++      --place le background au centre
        [ translate (fst pos) (snd pos) obsImg | Caillou c <- obs, let pos = toScreen (Ecran h w) c ] ++  --place les obs en convertissant les coord
        [ translate (fst pos) (snd pos) playerImg | let pos = toScreen (Ecran h w) playerCoord ] ++       --place la joueuse en convertissant les coord
        [ translate (-fromIntegral w * caseSize / 2) (fromIntegral h * caseSize / 2) $                    --affiche les pv en haut à gauche
           (scale 0.15 0.15 $
            color yellow $ 
            text ("PV: " ++ show pv))
        ] ++
        case st of
            Perdu -> [color orange $ text "GAME OVER!"]
            Gagne -> [color green $ text "GAGNE!"]
            _     -> []


-- Gère les entrées clavier (on délègue à gloss)
handleInput :: Event -> Envi -> Envi
handleInput (EventKey (Char c) Down _ _) env = actionEnv env c --délègue la gestion de l'action à actionEnv
handleInput _ env = env
--Down une touche à été pressée
-- _ _ représente les modificateurs ignorés (shift, ctrl...)



-- Met à jour l'environnement (est appelé automatiquement par gloss) (remplace faitTour)
updateEnv :: Float -> Envi -> Envi      --Float : temps écoulé en sec
updateEnv _ env =                       --mais on ne l'utilise pas
    let Etat f = do
            scroll            --fait un tour des actions de verification
            checkCollision      --de l'etat du jeu
            checkPerdu
            checkGagne
    in fst (f env)






-- =========================================================================
-- PARTIE 7 : PARTIE IMPURE (IO) ET MAIN
-- =========================================================================





-- renvoie un nombre au hasard entre a et b
randomInt :: Int -> Int -> IO Int           
randomInt a b = randomRIO (a, b)

-- Fonction qui renvoie une liste de n obstacle placés au hasard
-- comme on est dans Impur (IO) on doit utiliser return et do etc
-- le <- seulement pour les trucs impur je crois
listeObstacle :: Int -> Envi -> IO [Obstacle]
listeObstacle 0 _ = return []
listeObstacle n (Envi (Ecran ecrH ecrL) jo obs st) = do
    x <- (randomInt 0  (fromIntegral ecrL))  -- fromIntegral pour donner une Int a partir d'un Integer
    y <- (randomInt 0  (2 * (fromIntegral ecrH)))
    
    let new_caillou =  Caillou ( Coord  (toInteger x) (toInteger y) )
    suite <- listeObstacle (n-1) (Envi (Ecran ecrH ecrL) jo obs st)
    
    return (new_caillou : suite )





-- Fait tourner la monade pour avoir le nouvel environnement
--faitTour :: Char -> Envi -> IO Envi
--faitTour c env = do
--    let Etat p = tour c
--        (env', str) = p env
--    putStrLn str
--    return env'

-- La boucle de jeu infinie ( s'arrete si on perd)
--boucle :: Envi -> IO ()
--boucle env = case envSt env of
  --  Perdu -> putStrLn "=== BOUM ! TU AS PERDU ! GAME OVER ==="
  --  Gagne -> putStrLn "=== GANGE ! TU ES TROP FORTE ! ==="
  --  _     -> do 
  --      putStrLn "Appuie sur z, q, s, ou d puis Entree:"
  --      c <- getChar
        --_ <- getLine -- Pour absorber la touche Entree 
    --    env' <- faitTour c env
      --  boucle env'

-- le main de la partie 
main :: IO ()
main = do
    putStrLn "START"
    --On charge les images
    bgImg <- loadPNG "img/background.png"
    playerImg <- loadPNG "img/player.png"
    obsImg <- loadPNG "img/obs.png"

    putStrLn "START2"

    -- On fabrique notre partie de depart (Ecran 10x10, Joueuse en (5,0) avec 3 PV, et 3 Cailloux) 
    let ecr = Ecran 10 10
        jou = Joueuse( Coord(ecrL ecr `div` 2 ) 0 ) 3 -- Joueuse au centre en bas avec 3 PV
        envInit = Envi ecr jou [] EnCours
    
    --Génère 10 obstacles aléatoires
    liste_obs <- listeObstacle 10 envInit
    let partieInitiale = Envi ecr jou liste_obs EnCours

    -- Affiche un message de bienvenue dans la console
    putStrLn "=== BIENVENUE DANS PEW PEW ==="
    --putStrLn (show partieInitiale) -- On affiche l'etat de base
    -- on lance la boucle
    --boucle partieInitiale

    --On lance avec gloss
    play
        (InWindow "Pew Pew" (800, 600) (100, 100))  -- Fenêtre de 800x600 pixels
        black                                        -- Fond noir (remplacé par bgImg)
        5                                           -- 10 FPS (vitesse du jeu)
        partieInitiale                               -- État initial
        (renderEnv bgImg playerImg obsImg ecr)      -- Fonction de rendu
        handleInput                                  -- Gestion des entrées
        updateEnv                                    -- Mise à jour automatique






-- TODO: 
-- on pourrait faire genre niveau facile cest 10 obstacles et ca dre pas trop longtemps donc
-- l'obstacle le plus loin est a 2* ecrH
-- niveau intermediaire peutetre 30 obstacle et le plus loin sur 4* ecrH 
