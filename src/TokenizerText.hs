{-# LANGUAGE OverloadedStrings #-}

import Data.Char (isSpace)
import qualified Data.Text as T

tokenize :: T.Text -> [T.Text]
tokenize t0 =
  case T.uncons t1 of
    Nothing -> []
    Just (c0, t2) ->
      if c0 `elem` delims
        then (T.take 1 t1) : tokenize t2
        else
          let (t3, t4) = T.span (\c1 -> not (isSpace c1 || c1 `elem` delims)) t1
           in t3 : tokenize t4
  where
    delims = ",(){}:" :: String
    t1 = T.stripStart t0

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
