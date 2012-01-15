import System.Nemesis.Env

main = run nemesis

nemesis = do
  task "hello" $ do
    putStrLn "hello"
    sh "echo 'hello world'"

  task "crab" $ do
    putStrLn "Deploying to Crab"
    build
    sh "scp ./Main crab.local:/Users/seanhess/directory"
    sh "scp -r ./public crab.local:/Users/seanhess/"

build = do
    sh "ghc --make Main.hs"
    
