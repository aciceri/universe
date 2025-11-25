{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Brick
import Brick.Main (viewportScroll, vScrollToEnd)
import qualified Graphics.Vty as V
import qualified Graphics.Vty.CrossPlatform as VCP
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import Control.Monad (when, void)
import System.IO.Unsafe (unsafePerformIO)
import Language.Haskell.Interpreter hiding (get)
import qualified Language.Haskell.Interpreter as Hint (as)
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))
import System.Directory (findExecutable)
import Data.List (isPrefixOf, isInfixOf)
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, tryTakeMVar)
import Control.Concurrent (threadDelay)
import Lens.Micro ((^.))
import Lens.Micro.Mtl (use, (.=), (%=))
import Lens.Micro.TH (makeLenses)
import qualified TreeSitter.Parser as TS
import qualified TreeSitter.Tree as TS
import qualified TreeSitter.Node as TS
import qualified TreeSitter.Cursor as TSC
import qualified TreeSitter.Haskell as TSH
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (peek)
import Foreign.C.String (peekCString)

-- | Nome per i widget
data Name = InputEditor | OutputViewport
  deriving (Ord, Show, Eq)

-- | Stato dell'applicazione
data AppState = AppState
  { _inputBuffer :: T.Text        -- Testo corrente in input
  , _cursorPos :: Int             -- Posizione cursore
  , _outputLines :: [T.Text]      -- Output dei comandi eseguiti
  , _commandHistory :: [T.Text]   -- Storia comandi
  , _historyPos :: Int            -- Posizione nella storia (-1 = non in history)
  , _syntaxTree :: Maybe (Ptr TS.Tree)  -- Albero di parsing per highlighting
  , _haskellBindings :: [String]  -- Statement Haskell eseguiti (let, import, etc)
  }

makeLenses ''AppState

-- | Stato iniziale
initialState :: AppState
initialState = AppState
  { _inputBuffer = ""
  , _cursorPos = 0
  , _outputLines = ["Shelob - Haskell Shell", "Type :help for help", ""]
  , _commandHistory = []
  , _historyPos = -1
  , _syntaxTree = Nothing
  , _haskellBindings = initialHaskellBindings
  }

-- | Bindings Haskell iniziali (funzione 's' per eseguire comandi shell)
initialHaskellBindings :: [String]
initialHaskellBindings =
  [ "let s cmd = do { (code, out, err) <- readProcessWithExitCode \"sh\" [\"-c\", cmd] \"\"; return (out ++ err) }"
  ]

-- | Event handler
handleEvent :: BrickEvent Name e -> EventM Name AppState ()
handleEvent (VtyEvent ev) = case ev of
  -- Enter: esegui comando
  V.EvKey V.KEnter [] -> do
    input <- use inputBuffer
    when (not $ T.null $ T.strip input) $ do
      -- Aggiungi alla history
      commandHistory %= (input :)
      -- Ottieni bindings correnti
      bindings <- use haskellBindings
      -- Esegui comando
      (result, newBindings) <- liftIO $ executeCommand input bindings
      -- Aggiorna bindings se ci sono stati nuovi statement
      haskellBindings .= newBindings
      outputLines %= (++ ["> " <> input, result, ""])
      -- Scroll to bottom
      vScrollToEnd $ viewportScroll OutputViewport
      -- Reset input
      inputBuffer .= ""
      cursorPos .= 0
      historyPos .= (-1)
      syntaxTree .= Nothing

  -- Backspace
  V.EvKey V.KBS [] -> do
    pos <- use cursorPos
    buf <- use inputBuffer
    when (pos > 0) $ do
      let (before, after) = T.splitAt pos buf
          newBuf = T.init before <> after
      inputBuffer .= newBuf
      cursorPos %= subtract 1
      updateSyntaxTree newBuf

  -- Delete
  V.EvKey V.KDel [] -> do
    pos <- use cursorPos
    buf <- use inputBuffer
    when (pos < T.length buf) $ do
      let (before, after) = T.splitAt pos buf
          newBuf = before <> T.tail after
      inputBuffer .= newBuf
      updateSyntaxTree newBuf

  -- Arrow left
  V.EvKey V.KLeft [] -> do
    cursorPos %= max 0 . subtract 1

  -- Arrow right
  V.EvKey V.KRight [] -> do
    buf <- use inputBuffer
    cursorPos %= min (T.length buf) . (+1)

  -- Arrow up: history precedente
  V.EvKey V.KUp [] -> do
    hist <- use commandHistory
    hPos <- use historyPos
    let newPos = min (length hist - 1) (hPos + 1)
    when (newPos >= 0 && newPos < length hist) $ do
      historyPos .= newPos
      let cmd = hist !! newPos
      inputBuffer .= cmd
      cursorPos .= T.length cmd
      updateSyntaxTree cmd

  -- Arrow down: history successiva
  V.EvKey V.KDown [] -> do
    hPos <- use historyPos
    hist <- use commandHistory
    if hPos <= 0
      then do
        historyPos .= (-1)
        inputBuffer .= ""
        cursorPos .= 0
        syntaxTree .= Nothing
      else do
        let newPos = hPos - 1
        historyPos .= newPos
        let cmd = hist !! newPos
        inputBuffer .= cmd
        cursorPos .= T.length cmd
        updateSyntaxTree cmd

  -- Home
  V.EvKey V.KHome [] -> cursorPos .= 0

  -- End
  V.EvKey V.KEnd [] -> do
    buf <- use inputBuffer
    cursorPos .= T.length buf

  -- Ctrl-C o Ctrl-D: esci
  V.EvKey (V.KChar 'c') [V.MCtrl] -> halt
  V.EvKey (V.KChar 'd') [V.MCtrl] -> do
    buf <- use inputBuffer
    when (T.null buf) halt

  -- Caratteri normali
  V.EvKey (V.KChar c) [] -> do
    pos <- use cursorPos
    buf <- use inputBuffer
    let (before, after) = T.splitAt pos buf
        newBuf = before <> T.singleton c <> after
    inputBuffer .= newBuf
    cursorPos %= (+1)
    updateSyntaxTree newBuf

  _ -> return ()

handleEvent _ = return ()

-- | Aggiorna l'albero di sintassi con tree-sitter
updateSyntaxTree :: T.Text -> EventM Name AppState ()
updateSyntaxTree _input = do
  -- Disabilitato: causa blocchi con unsafePerformIO nel rendering
  syntaxTree .= Nothing

-- | Parsa codice Haskell con tree-sitter (con cleanup)
parseHaskell :: T.Text -> IO (Maybe (Ptr TS.Tree))
parseHaskell input = do
  parser <- TS.ts_parser_new
  success <- TS.ts_parser_set_language parser TSH.tree_sitter_haskell
  if not success
    then do
      TS.ts_parser_delete parser
      return Nothing
    else do
      let bytes = TE.encodeUtf8 input
      tree <- BS.useAsCStringLen bytes $ \(cstr, len) ->
        TS.ts_parser_parse_string parser nullPtr cstr (fromIntegral len)
      TS.ts_parser_delete parser  -- Libera il parser
      if tree == nullPtr
        then return Nothing
        else return (Just tree)

-- | Pre-processa codice Haskell sostituendo comandi shell con s "cmd"
-- Esegue tree-sitter in un thread separato per non bloccare Brick
preprocessHaskellCode :: T.Text -> IO T.Text
preprocessHaskellCode input = do
  resultVar <- newEmptyMVar

  -- Fork thread separato per tree-sitter
  _ <- forkIO $ do
    result <- parseAndSubstitute input
    putMVar resultVar result

  -- Aspetta con timeout (1 secondo = 1000000 microseconds)
  threadDelay 1000000
  mbResult <- tryTakeMVar resultVar

  case mbResult of
    Just result -> return result
    Nothing -> return input  -- Timeout dopo 1 secondo, ritorna input invariato

-- | Parsa e sostituisce (da eseguire in thread separato)
parseAndSubstitute :: T.Text -> IO T.Text
parseAndSubstitute input = do
  mbTree <- parseHaskell input
  case mbTree of
    Nothing -> return input
    Just tree -> do
      substitutions <- findShellCommandsWithLimit tree input 100
      TS.ts_tree_delete tree
      return $ applySubstitutions input (reverse substitutions)

-- | Token semplice per preprocessing
data SimpleToken = SimpleToken
  { stText :: T.Text
  , stIsIdentifier :: Bool
  , stWasSubstituted :: Bool
  }

-- | Processa i token controllando quali sono comandi shell
processTokens :: [SimpleToken] -> IO [SimpleToken]
processTokens [] = return []
processTokens (tok:rest)
  | not (stIsIdentifier tok) = do
      restProcessed <- processTokens rest
      return (tok : restProcessed)
  | stText tok `elem` skipIdentifiers = do
      restProcessed <- processTokens rest
      return (tok : restProcessed)
  | otherwise = do
      mbExec <- findExecutable (T.unpack $ stText tok)
      restProcessed <- processTokens rest
      case mbExec of
        Just _ -> return (tok { stWasSubstituted = True } : restProcessed)
        Nothing -> return (tok : restProcessed)

-- | Identificatori da non sostituire
skipIdentifiers :: [T.Text]
skipIdentifiers = ["let", "in", "where", "case", "of", "if", "then", "else",
                   "data", "type", "newtype", "class", "instance", "module",
                   "import", "qualified", "as", "hiding", "do", "deriving",
                   "return", "forall", "foreign", "s",
                   -- Funzioni Prelude comuni
                   "map", "filter", "foldr", "foldl", "foldl1", "foldr1",
                   "take", "drop", "head", "tail", "reverse", "length",
                   "zip", "unzip", "concat", "concatMap", "sum", "product",
                   "maximum", "minimum", "lines", "unlines", "words", "unwords",
                   "print", "putStr", "putStrLn", "getLine", "readFile", "writeFile",
                   "show", "read", "fmap", "pure", "sequence", "mapM", "mapM_",
                   "maybe", "either", "null", "elem", "notElem", "all", "any",
                   "and", "or", "not", "fst", "snd", "id", "const", "flip",
                   "until", "iterate", "repeat", "replicate", "cycle",
                   "splitAt", "takeWhile", "dropWhile", "span", "break",
                   "zipWith", "lookup"]

-- | Tokenizza semplice per preprocessing
simpleTokenizeForPreproc :: T.Text -> [SimpleToken]
simpleTokenizeForPreproc text = go text
  where
    go t
      | T.null t = []
      | isIdentStart (T.head t) =
          let (ident, rest) = T.span isIdentChar t
          in SimpleToken ident True False : go rest
      | otherwise =
          let (other, rest) = T.span (not . isIdentStart) t
          in SimpleToken other False False : go rest

    isIdentStart c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_'
    isIdentChar c = isIdentStart c || (c >= '0' && c <= '9') || c == '\''

-- | Rappresenta una sostituzione da fare nel codice
data Substitution = Substitution
  { substStart :: Int
  , substEnd :: Int
  , substNew :: T.Text
  }

-- | Applica sostituzioni al testo
applySubstitutions :: T.Text -> [Substitution] -> T.Text
applySubstitutions text [] = text
applySubstitutions text (Substitution start end new : rest) =
  let bytes = TE.encodeUtf8 text
      before = TE.decodeUtf8 $ BS.take start bytes
      after = TE.decodeUtf8 $ BS.drop end bytes
      newText = before <> new <> after
  in applySubstitutions newText rest

-- | Trova comandi shell nell'AST che vanno sostituiti (con limite di nodi)
findShellCommandsWithLimit :: Ptr TS.Tree -> T.Text -> Int -> IO [Substitution]
findShellCommandsWithLimit treePtr input maxNodes = do
  alloca $ \nodePtr -> do
    TS.ts_tree_root_node_p treePtr nodePtr
    alloca $ \tsNodePtr -> do
      TS.ts_node_poke_p tsNodePtr nodePtr
      TSC.withCursor tsNodePtr $ \cursorPtr -> do
        collectVariablesWithLimit input cursorPtr maxNodes

-- | Trova comandi shell nell'AST che vanno sostituiti
findShellCommands :: Ptr TS.Tree -> T.Text -> IO [Substitution]
findShellCommands treePtr input = findShellCommandsWithLimit treePtr input 10000

-- | Attraversa l'albero con limite di nodi
collectVariablesWithLimit :: T.Text -> Ptr TSC.Cursor -> Int -> IO [Substitution]
collectVariablesWithLimit input cursorPtr maxNodes = go [] 0
  where
    go acc count
      | count >= maxNodes = return acc
      | otherwise = do
          alloca $ \nodePtr -> do
            success <- TSC.ts_tree_cursor_current_node_p cursorPtr nodePtr
            if not success
              then return acc
              else do
                node <- peek nodePtr
                nodeTypeCStr <- return $ TS.nodeType node
                nodeTypeStr <- peekCString nodeTypeCStr
                let nodeTyp = nodeTypeStr
                    startByte = fromIntegral $ TS.nodeStartByte node
                    endByte = fromIntegral $ TS.nodeEndByte node
                    isNamed = TS.nodeIsNamed node /= 0

                newAcc <- if isNamed && shouldSubstitute nodeTyp
                  then do
                    let varName = extractTextRange input startByte endByte
                    if varName `elem` skipIdentifiers
                      then return acc
                      else do
                        mbExec <- findExecutable (T.unpack varName)
                        case mbExec of
                          Just _ -> return (Substitution startByte endByte ("s \"" <> varName <> "\"") : acc)
                          Nothing -> return acc
                  else return acc

                hasChild <- TSC.ts_tree_cursor_goto_first_child cursorPtr
                if hasChild
                  then go newAcc (count + 1)
                  else tryNextSibling newAcc (count + 1)

    tryNextSibling acc count = do
      hasNext <- TSC.ts_tree_cursor_goto_next_sibling cursorPtr
      if hasNext
        then go acc count
        else tryParent acc count

    tryParent acc count = do
      hasParent <- TSC.ts_tree_cursor_goto_parent cursorPtr
      if hasParent
        then tryNextSibling acc count
        else return acc

-- | Attraversa l'albero e raccoglie variabili da sostituire (non ricorsivo)
collectVariablesForSubst :: T.Text -> Ptr TSC.Cursor -> IO [Substitution]
collectVariablesForSubst input cursorPtr = collectVariablesWithLimit input cursorPtr 10000

-- | Determina se un nodo dovrebbe essere sostituito
shouldSubstitute :: String -> Bool
shouldSubstitute nodeType =
  nodeType == "variable" || nodeType == "variable_identifier"
  -- TODO: migliorare filtrando contesti di definizione (let bindings, pattern matching, etc)

-- | Esegui comando (Haskell o shell)
-- Ritorna (risultato, nuovi bindings)
executeCommand :: T.Text -> [String] -> IO (T.Text, [String])
executeCommand input bindings
  | T.null (T.strip input) = return ("", bindings)
  | T.isPrefixOf ":" input = do
      res <- executeSpecialCommand input
      return (res, bindings)
  | otherwise = do
      -- Pre-processa il codice sostituendo comandi shell
      preprocessed <- preprocessHaskellCode input
      -- Esegui come Haskell
      result <- runInterpreter $ do
        setImports ["Prelude", "Data.List", "Data.Maybe", "Control.Monad", "System.Process", "System.Exit"]
        -- Riesegui tutti i bindings precedenti
        mapM_ runStmt bindings
        -- Prova prima come statement
        if T.isPrefixOf "let " preprocessed || T.isPrefixOf "import " preprocessed
          then do
            runStmt (T.unpack preprocessed)
            return "<statement executed>"
          else do
            -- Controlla il tipo per determinare se è IO
            ty <- typeOf (T.unpack preprocessed)
            if "IO" `isInfixOf` ty
              then do
                -- È un'azione IO, eseguila
                action <- interpret (T.unpack preprocessed) (Hint.as :: IO String)
                liftIO action
              else do
                -- È un'espressione pura, valutala
                eval (T.unpack preprocessed)
      case result of
        Right val ->
          -- Se era uno statement, aggiungi ai bindings (usa input originale)
          if T.isPrefixOf "let " input || T.isPrefixOf "import " input
            then return (T.pack val, bindings ++ [T.unpack preprocessed])
            else return (T.pack val, bindings)
        Left err ->
          -- Mostra errore Haskell
          return (T.pack $ formatError err, bindings)

-- | Estrae il nome della variabile da un errore "Not in scope"
-- Esempi di errori GHC:
--   "Variable not in scope: ls"
--   "Not in scope: 'ls'"
--   "Not in scope: ls"
--   "<interactive>:1:1: error: Not in scope: 'git'"
extractNotInScopeVar :: String -> Maybe String
extractNotInScopeVar err =
  -- Prova diversi formati di errore
  let prefixes = ["Variable not in scope:", "Not in scope:"]
      tryPrefix prefix = case findSubstring prefix err of
        Just idx ->
          let afterPrefix = drop (idx + length prefix) err
              -- Rimuovi eventuali quote e spazi e prendi la prima parola
              quotes = " '\"`" :: String
              cleaned = dropWhile (`elem` quotes) afterPrefix
              varName = takeWhile (\c -> c /= '\'' && c /= '"' && c /= ' ' && c /= '\n' && c /= ':') cleaned
          in if null varName then Nothing else Just varName
        Nothing -> Nothing
  in case mapMaybe tryPrefix prefixes of
       (name:_) -> Just name
       [] -> Nothing
  where
    findSubstring :: String -> String -> Maybe Int
    findSubstring needle haystack =
      let len = length needle
          indices = [0 .. length haystack - len]
      in case filter (\i -> take len (drop i haystack) == needle) indices of
           (i:_) -> Just i
           [] -> Nothing

    mapMaybe :: (a -> Maybe b) -> [a] -> [b]
    mapMaybe f = foldr (\x acc -> case f x of Just y -> y : acc; Nothing -> acc) []

-- | Esegui comandi speciali (:help, :quit, :type)
executeSpecialCommand :: T.Text -> IO T.Text
executeSpecialCommand cmd
  | cmd == ":help" || cmd == ":h" = return $ T.unlines
      [ "Shelob - Haskell Shell"
      , ""
      , "You can type:"
      , "  - Haskell expressions: map (*2) [1..5]"
      , "  - Shell commands: ls, pwd, git status"
      , ""
      , "Special commands:"
      , "  :help, :h      Show this help"
      , "  :quit, :q      Exit the shell"
      , "  :type <expr>   Show type of expression"
      , ""
      , "Navigation:"
      , "  Ctrl-C/D       Exit"
      , "  Arrow keys     Navigate input"
      , "  Up/Down        Command history"
      ]
  | cmd == ":quit" || cmd == ":q" = return ":quit"
  | T.isPrefixOf ":type " cmd || T.isPrefixOf ":t " cmd = do
      -- TODO: :type dovrebbe usare i bindings, ma per ora funziona solo con prelude
      let expr = T.unpack $ T.strip $ T.drop (if T.isPrefixOf ":type" cmd then 6 else 3) cmd
      result <- runInterpreter $ do
        setImports ["Prelude", "Data.List", "Data.Maybe", "Control.Monad", "System.Process", "System.Exit"]
        typeOf expr
      case result of
        Right ty -> return $ T.pack $ expr ++ " :: " ++ ty
        Left err -> return $ T.pack $ formatError err
  | otherwise = return $ "Unknown command: " <> cmd

-- | Formatta errori dell'interprete
formatError :: InterpreterError -> String
formatError (WontCompile errs) = unlines (map errMsg errs)
formatError (UnknownError msg) = "Unknown error: " ++ msg
formatError (NotAllowed msg) = "Not allowed: " ++ msg
formatError (GhcException msg) = "GHC exception: " ++ msg

-- | Rendering UI
drawUI :: AppState -> [Widget Name]
drawUI st = [ui]
  where
    ui = vBox
      [ -- Tutto in un unico pannello scrollabile
        viewport OutputViewport Vertical $ vBox $
          map txt (st ^. outputLines) ++ [renderInput st]
      ]

-- | Renderizza l'input con syntax highlighting
renderInput :: AppState -> Widget Name
renderInput st =
  let buf = st ^. inputBuffer
      pos = st ^. cursorPos
      prompt = txt "shadowfax> "
      -- Usa highlighting con tree-sitter se disponibile
      inputWidget = if T.null buf
                    then showCursor InputEditor (Location (0, 0)) (txt " ")
                    else case st ^. syntaxTree of
                      Nothing -> renderWithSimpleHighlighting buf pos
                      Just tree -> renderWithTreeSitter buf pos tree
  in prompt <+> inputWidget

-- | Rendering con tree-sitter
renderWithTreeSitter :: T.Text -> Int -> Ptr TS.Tree -> Widget Name
renderWithTreeSitter buf pos tree =
  let tokens = extractTokensFromTree buf tree
      widgets = renderTokensWithCursor tokens pos
  in hBox widgets

-- | Rendering con syntax highlighting semplice (fallback)
renderWithSimpleHighlighting :: T.Text -> Int -> Widget Name
renderWithSimpleHighlighting buf pos =
  let tokens = simpleTokenize buf
      widgets = renderTokensWithCursor tokens pos
  in hBox widgets

-- | Rendering semplice senza highlighting
renderSimple :: T.Text -> Int -> Widget Name
renderSimple buf pos =
  let (before, after) = T.splitAt pos buf
  in if T.null after
     -- Cursore alla fine: mostra spazio come cursore
     then txt before <+> showCursor InputEditor (Location (0, 0)) (txt " ")
     -- Cursore nel mezzo: evidenzia carattere corrente
     else let cursorChar = T.take 1 after
              rest = T.drop 1 after
          in txt before <+> showCursor InputEditor (Location (0, 0)) (txt cursorChar) <+> txt rest

-- | Rendering con syntax highlighting usando tree-sitter
renderHighlighted :: T.Text -> Int -> Ptr TS.Tree -> Widget Name
renderHighlighted buf pos _treePtr =
  -- Per ora: rendering semplice colorato carattere per carattere
  let chars = T.unpack buf
      charPositions = [0..length chars - 1]
      widgets = map (\(c, p) -> renderCharAtPos c p pos) (zip chars charPositions)
  in if null widgets
     then showCursor InputEditor (Location (0, 0)) (txt " ")
     else hBox widgets
  where
    renderCharAtPos c p curPos
      | p == curPos = showCursor InputEditor (Location (0, 0)) (txt $ T.singleton c)
      | otherwise = txt $ T.singleton c

-- | Token con tipo e range
data Token = Token
  { tokenText :: T.Text
  , tokenType :: T.Text
  , tokenStart :: Int
  , tokenEnd :: Int
  } deriving (Show)

-- | Estrae i token dall'albero tree-sitter
extractTokensFromTree :: T.Text -> Ptr TS.Tree -> [Token]
extractTokensFromTree buf treePtr = unsafePerformIO $ do
  alloca $ \nodePtr -> do
    TS.ts_tree_root_node_p treePtr nodePtr
    rootNode <- peek nodePtr
    -- Usa il Cursor per attraversare l'albero
    alloca $ \tsNodePtr -> do
      TS.ts_node_poke_p tsNodePtr nodePtr
      TSC.withCursor tsNodePtr $ \cursorPtr -> do
        collectTokensWithCursor buf cursorPtr

-- | Attraversa l'albero usando un Cursor e raccoglie i token
collectTokensWithCursor :: T.Text -> Ptr TSC.Cursor -> IO [Token]
collectTokensWithCursor buf cursorPtr = do
  -- Ottieni il nodo corrente
  alloca $ \nodePtr -> do
    success <- TSC.ts_tree_cursor_current_node_p cursorPtr nodePtr
    if not success
      then return []
      else do
        node <- peek nodePtr
        let startByte = fromIntegral $ TS.nodeStartByte node
            endByte = fromIntegral $ startByte + fromIntegral (TS.nodeEndByte node - TS.nodeStartByte node)
            childCount = TS.nodeChildCount node

        -- Ottieni tipo nodo
        nodeTypeCStr <- return $ TS.nodeType node
        nodeTypeStr <- peekCString nodeTypeCStr
        let nodeTyp = T.pack nodeTypeStr

        if childCount == 0 && TS.nodeIsNamed node /= 0
          then do
            -- Nodo foglia: crea token
            let tokenText = extractTextRange buf startByte endByte
                tokenTyp = mapTreeSitterType nodeTyp
            return [Token tokenText tokenTyp startByte endByte]
          else if childCount == 0
            then do
              -- Nodo anonimo
              let tokenText = extractTextRange buf startByte endByte
              return [Token tokenText "default" startByte endByte]
            else do
              -- Nodo con figli: attraversa
              hasChild <- TSC.ts_tree_cursor_goto_first_child cursorPtr
              if not hasChild
                then return []
                else do
                  firstTokens <- collectTokensWithCursor buf cursorPtr
                  siblingTokens <- collectSiblings buf cursorPtr
                  _ <- TSC.ts_tree_cursor_goto_parent cursorPtr
                  return (firstTokens ++ siblingTokens)

-- | Raccogli token dai fratelli
collectSiblings :: T.Text -> Ptr TSC.Cursor -> IO [Token]
collectSiblings buf cursorPtr = do
  hasNext <- TSC.ts_tree_cursor_goto_next_sibling cursorPtr
  if not hasNext
    then return []
    else do
      tokens <- collectTokensWithCursor buf cursorPtr
      moreTokens <- collectSiblings buf cursorPtr
      return (tokens ++ moreTokens)

-- | Estrai testo da un range di byte
extractTextRange :: T.Text -> Int -> Int -> T.Text
extractTextRange text start end =
  let bytes = TE.encodeUtf8 text
      rangeBytes = BS.take (end - start) $ BS.drop start bytes
  in TE.decodeUtf8 rangeBytes

-- | Mappa tipi tree-sitter Haskell a categorie
mapTreeSitterType :: T.Text -> T.Text
mapTreeSitterType typ = case typ of
  -- Keywords
  t | t `elem` ["let", "in", "where", "case", "of", "if", "then", "else",
                "data", "type", "newtype", "class", "instance", "module",
                "import", "qualified", "as", "hiding", "do", "deriving",
                "forall", "foreign"] -> "keyword"
  -- Literals
  "string" -> "string"
  "char" -> "string"
  "integer" -> "number"
  "float" -> "number"
  -- Comments
  "comment" -> "comment"
  -- Operators
  "operator" -> "operator"
  "constructor_operator" -> "operator"
  -- Types
  "type_constructor" -> "type"
  -- Functions/variables
  "variable" -> "function"
  _ -> "default"

-- | Tokenizzazione semplice basata su pattern (fallback)
-- Usato quando tree-sitter fallisce o per testo non valido
simpleTokenize :: T.Text -> [Token]
simpleTokenize text = go 0 text
  where
    go pos t
      | T.null t = []
      | T.head t == ' ' = Token " " "default" pos (pos + 1) : go (pos + 1) (T.tail t)
      | otherwise =
          let (word, rest) = T.span (/= ' ') t
              len = T.length word
              typ = classifyWord word
          in Token word typ pos (pos + len) : go (pos + len) rest

    classifyWord w
      | w `elem` keywords = "keyword"
      | T.length w > 0 && T.all isOperatorChar w = "operator"
      | T.isPrefixOf "\"" w = "string"
      | T.isPrefixOf "'" w = "string"
      | T.length w > 0 && T.all (\c -> (c >= '0' && c <= '9') || c == '.') w = "number"
      | T.isPrefixOf "--" w = "comment"
      | otherwise = "default"

    isOperatorChar c = c `elem` ['=', '-', '>', '<', '|', ':', '\\', '@', '+', '*', '/', '!', '$', '%', '&', '.', ',']

    keywords = ["let", "in", "where", "case", "of", "if", "then", "else",
                "data", "type", "newtype", "class", "instance", "module",
                "import", "qualified", "as", "hiding", "do", "deriving"]

-- | Renderizza i token con gestione cursore
renderTokensWithCursor :: [Token] -> Int -> [Widget Name]
renderTokensWithCursor tokens cPos =
  let (beforeCursor, atAndAfter) = break (\t -> tokenEnd t > cPos) tokens
      -- Render tokens prima del cursore
      beforeWidgets = map renderToken beforeCursor
      -- Gestisci token al cursore
      cursorWidgets = case atAndAfter of
        [] -> [showCursor InputEditor (Location (0, 0)) (txt " ")]
        (t:rest) ->
          let relPos = cPos - tokenStart t
              (before, after) = T.splitAt relPos (tokenText t)
              cursorChar = if T.null after then " " else T.take 1 after
              afterChar = T.drop 1 after
              attr = nodeTypeToAttr (tokenType t)
          in [ withAttr attr (txt before)
             , showCursor InputEditor (Location (0, 0)) (withAttr attr (txt cursorChar))
             , withAttr attr (txt afterChar)
             ] ++ map renderToken rest
  in beforeWidgets ++ cursorWidgets

-- | Renderizza un singolo token con il suo colore
renderToken :: Token -> Widget Name
renderToken (Token text typ _ _) =
  withAttr (nodeTypeToAttr typ) (txt text)

-- | Mappa tipo di nodo tree-sitter ad attributo Brick
nodeTypeToAttr :: T.Text -> AttrName
nodeTypeToAttr nodeType = case nodeType of
  "string" -> attrName "string"
  "number" -> attrName "number"
  "comment" -> attrName "comment"
  "keyword" -> attrName "keyword"
  "operator" -> attrName "operator"
  "function" -> attrName "function"
  "type" -> attrName "type"
  "variable" -> attrName "variable"
  _ -> attrName "default"

-- | Attribute map per i colori
theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (attrName "default", V.defAttr)
  , (attrName "keyword", fg V.blue `V.withStyle` V.bold)
  , (attrName "string", fg V.green)
  , (attrName "number", fg V.cyan)
  , (attrName "comment", fg V.brightBlack)
  , (attrName "operator", fg V.yellow)
  , (attrName "function", fg V.magenta `V.withStyle` V.bold)
  , (attrName "type", fg V.yellow `V.withStyle` V.bold)
  , (attrName "variable", V.defAttr)
  ]

-- | App definition
app :: App AppState e Name
app = App
  { appDraw = drawUI
  , appChooseCursor = showFirstCursor
  , appHandleEvent = handleEvent
  , appStartEvent = return ()
  , appAttrMap = const theMap
  }

-- | Main
main :: IO ()
main = do
  initialVty <- VCP.mkVty V.defaultConfig
  let buildVty = VCP.mkVty V.defaultConfig
  void $ customMain initialVty buildVty Nothing app initialState
