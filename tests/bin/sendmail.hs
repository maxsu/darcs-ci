import System.Environment

main = do
  args <- getArgs
  let filename = args !! 5
  input <- getContents
  filecontent <- readFile filename
  writeFile "saved.out" $
    unlines $
    [ "arg["++show i++"] = "++v | (i,v) <- zip [1..] args ]
    ++
    [ "input contains:"
    , input
    , filename++" contains:"
    , filecontent
    ]
