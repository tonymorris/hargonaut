module Data.Json.Parse where

import Control.Category(Category(id))
import Control.Applicative
import Control.Monad(Monad(return, fail), void, replicateM, (=<<))
import Data.Bool(Bool(False, True))
import Data.Char(Char)
import Data.Functor(Functor(fmap), (<$>))
import Data.Int(Int)
import Data.List(elem, notElem)
import Data.Maybe(Maybe, maybe)
import Data.String(String)
import Numeric(readHex)
import Prelude(show)
import Text.Parser.Char
import Text.Parser.Combinators


{-

// def repsep[T](p: ⇒ Parser[T], q: ⇒ Parser[Any]): Parser[List[T]] 
// A parser generator for interleaved repetitions.

// (Parser[T])def <~[U](q: ⇒ Parser[U]): Parser[T] 
// A parser combinator for sequential composition which keeps only the left result.

// (Parser[T])def ~>[U](q: ⇒ Parser[U]): Parser[U] 
// A parser combinator for sequential composition which keeps only the right result.

// (Parser[T])def ^^[U](f: (T) ⇒ U): Parser[U] 
// A parser combinator for function application.

// (Parser[T])def ^^^[U](v: ⇒ U): Parser[U] 
// A parser combinator that changes a successful result into the specified value.

// (Parser[T])def |||[U >: T](q0: ⇒ Parser[U]): Parser[U]
// A parser combinator for alternative with longest match composition.

// (Parser[T])def |[U >: T](q: ⇒ Parser[U]): Parser[U]
// A parser combinator for alternative composition.

// def acceptSeq[ES](es: ES)(implicit arg0: (ES) ⇒ Iterable[Elem]): Parser[List[Elem]]
// A parser that matches only the given scala.collection.Iterable collection of elements es.

// def ~[U](q: ⇒ Parser[U]): Parser[~[T, U]]
// A parser combinator for sequential composition.

// (Parser[T])def *: Parser[List[T]]
// Returns a parser that repeatedly parses what this parser parses. 

// (Parser[T])def ?: Parser[Option[T]]
// Returns a parser that optionally parses what this parser parses.

package com.ephox
package argonaut

import util.parsing.combinator._
import Json._
import JsonNumber._

class JsonParser extends Parsers {
  type Elem = Char

  def jobject: Parser[Json] = openobject ~> repsep(pair, separator) <~ trailingcomma <~ closeobject ^^ jObjectAssocList

  def jarray: Parser[Json] = openarray ~> repsep(jvalue, separator) <~ trailingcomma <~ closearray ^^ jArray

  def jvalue: Parser[Json] = whitespace ~> (jobject ||| jarray ||| jstring ||| jboolean ||| jnull |||  jnumber) <~ whitespace

  def jstring = string ^^ jString

  def jnumber = number ^^ jNumber

  def jnull = acceptSeq("null") ^^^ jNull

  def jboolean = (f | t) ^^ jBool

  def trailingcomma = ((whitespace ~ ',')?)
-}

openarray ::
  CharParsing m =>
  m Char
openarray =
  char '[' <* spaces

closearray ::
  CharParsing m =>
  m Char
closearray =
  spaces *> char ']'

openobject ::
  CharParsing m =>
  m Char
openobject =
  char '{' <* spaces

closeobject ::
  CharParsing m =>
  m Char
closeobject =
  spaces *> char '}'

separator ::
  CharParsing m =>
  m ()
separator =
  void (spaces *> char ',' <* spaces)

colonSeparator ::
  CharParsing m =>
  m ()
colonSeparator =
  void (spaces *> char ':' <* spaces)

false ::
  CharParsing m =>
  m Bool
false =
  False <$ string "false"

true ::
  CharParsing m =>
  m Bool
true =
  True <$ string "true"

str ::
  CharParsing m =>
  m String
str =
  between (char '"') (char '"') (many anyChar)

unicode ::
  (Monad m, CharParsing m) =>
  m String
unicode =
  let r s = case readHex s of
              []  ->
                fail "failed to parse hex"
              (x,_):[] ->
                return (show (x :: Int))
              _:_:_ ->
                fail "ambiguously parsed hex"
  in r =<< replicateM 4 hexDigit

jchar ::
  (Monad m, CharParsing m) =>
  m String
jchar =
  let escape c = char '\\' *> char c
  in (:[]) <$> satisfy (`notElem` ['\\', '"']) <|>
     "\"" <$ escape '\"' <|>
     "\\" <$ escape '\\' <|>
     "/"  <$ escape '/'  <|>
     "\b" <$ escape 'b'  <|>
     "\f" <$ escape 'f'  <|>
     "\n" <$ escape 'n'  <|>
     "\r" <$ escape 'r'  <|>
     "\t" <$ escape 't'  <|>
     char '\\' *> unicode

undef = undef

notzerodigit ::
  CharParsing m =>
  m Char
notzerodigit =
  satisfy (`elem` ['1'..'9'])

sign ::
  CharParsing m =>
  m (Maybe Char)
sign =
  optional (char '-')

digitseries ::
  CharParsing m =>
  m String
digitseries =
  liftA2 (:) notzerodigit (many digit)

int ::
  CharParsing m =>
  m String
int =
  liftA2 (maybe id (:)) sign (digitseries <|> pure <$> digit)

  {-}

  def pair: Parser[(String, Json)] = (string <~ colanSeparator) ~ jvalue ^^ { case k ~ v => (k, v)}

  def number = (int ||| intfrac ||| intexp ||| intfracexp) ^^ {q => JsonNumber(q.mkString.toDouble)}

  def intexp = int ~ exp ^^ {case a ~ b => a ++ b}

  def intfracexp = int ~ frac ~ exp ^^ {case a ~ b ~ c => a ++ b ++ c}

  def intfrac = int ~ frac ^^ {case a ~ b => a ++ b}

  def exp = e ~ digits ^^ {case a ~ b => a ++ b}

  def frac = '.' ~ digits ^^ {case a ~ b => a :: b}

  def int = sign ~ (digitSeries | digitSingle) ^^ {
    case Some(s) ~ a => s :: a
    case None ~ a => a
  }

  def sign = ('-'?)

  def digits = digit+

  def digitSeries = nonzero ~ (digit*) ^^ {case a ~ b => a :: b}

  def digitSingle = digit map (List(_))

  def digit = elem("digit", _.isDigit)

  def nonzero = elem("nonzero", d => d.isDigit && d != '0')

  def e = List("e", "e+", "e-", "E", "E+", "E-") map (acceptSeq (_:String)) reduceRight (_ ||| _)
}

-}
