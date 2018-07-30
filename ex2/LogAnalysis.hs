module LogAnalysis where

    import Log

    -- exercise 1
    parse_message :: [String] -> LogMessage
    parse_message (('I':[]):t:text) = LogMessage Info (read t :: Int) (unwords text)
    parse_message (('W':[]):t:text) = LogMessage Warning (read t :: Int) (unwords text)
    parse_message (('E':[]):s:t:text) = LogMessage (Error (read s :: Int)) (read t :: Int) (unwords text)
    parse_message message_words = Unknown (unwords message_words)

    parse :: String -> [LogMessage]
    parse input = map parse_message (map words (lines input))

    -- exercise 2
    insert :: LogMessage -> MessageTree -> MessageTree
    insert msg@(LogMessage _ _ _) tree@(Leaf) = Node Leaf msg Leaf
    insert imsg@(LogMessage _ its _) tree@(Node l nmsg@(LogMessage _ nts _) r)
        | its > nts = Node l nmsg (insert imsg r)
        | otherwise = Node l imsg tree
    insert _ tree = tree

    --exercise 3
    build :: [LogMessage] -> MessageTree
    build [] = Leaf
    build (x:xs) = insert x (build xs)

    --exercise 4
    inOrder :: MessageTree -> [LogMessage]
    inOrder tree@(Leaf) = []
    inOrder tree@(Node l msg r) = (inOrder l) ++ [msg] ++ (inOrder r)

    --exercise 5
    messageIsImportant :: LogMessage -> Bool
    messageIsImportant (LogMessage (Error severity) _ _) = severity >= 50
    messageIsImportant _ = False

    whatWentWrong :: [LogMessage] -> [String]
    whatWentWrong messages = map show (filter messageIsImportant (inOrder (build messages)))
