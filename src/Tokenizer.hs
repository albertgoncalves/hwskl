tokenize :: String -> [String]
tokenize [] = []
tokenize (c : cs)
  | c `elem` space = tokenize cs
  | c `elem` delims = [c] : tokenize cs
  | otherwise = (c : token) : tokenize rest
  where
    (token, rest) = span (`notElem` (space ++ delims)) cs
    space = " \n\r\t"
    delims = ",(){}:"

main :: IO ()
main =
  mapM_ print $
    tokenize
      "version 49, 0\n\
      \class final super java/lang/Object Hello {\n\
      \    Hello ()V {\n\
      \            aload           this\n\
      \            invokespecial   java/lang/Object, <init>, ()V\n\
      \            return\n\
      \    }\n\
      \    static main (args [Ljava/lang/String;)V {\n\
      \            iconst          0\n\
      \        {\n\
      \            istore          i\n\
      \        for:\n\
      \            iload           i\n\
      \            aload           args\n\
      \            arraylength\n\
      \            if_icmpge       break\n\
      \            getstatic       java/lang/System, out, Ljava/io/PrintStream;\n\
      \            aload           args\n\
      \            iload           i\n\
      \            aaload\n\
      \            invokevirtual   java/io/PrintStream, println, (Ljava/lang/String;)V\n\
      \            iinc            i, 1\n\
      \            goto            for\n\
      \        }\n\
      \        break:\n\
      \            return\n\
      \    }\n\
      \}"
