module QuizMarker where

import Data.Maybe(isJust)
import Data.List(inits,nub)
import Data.Char(isSpace,isDigit)
import Data.Foldable(find)
import Text.Read(readMaybe)
import Data.Time.Format
import Data.Time.Clock
import Test.QuickCheck
import Control.Monad (when, guard)
import Data.Maybe (fromMaybe, fromJust)

import Numeric (showFFloat)
import Data.Bifunctor (second)
import Data.List(intercalate)

{- A `Parser a` consumes input (of type `String`),
   and can either fail (represented by returning `Nothing`)
   or produce a value of type `a` along with a `String`
   of leftovers -- this is the unconsumed suffix
   of the original input.

   Side note:
    This combines features of both the Maybe monad and the
    State monad. So why didn't we just use those?
    Well, note that  `State String (Maybe a)`
    isn't quite what we want (because then failure
    could consume input), and neither is
    `Maybe (State String a)` (because then failure
    could not depend on what the input is).
 -}
newtype Parser a = Parser (String -> Maybe (String,a))

instance Functor Parser where
  fmap f (Parser p) = Parser $ fmap (fmap f) . p

instance Applicative Parser where
  pure x = Parser $ \s -> Just (s,x)
  Parser f <*> Parser x =
     Parser $ \s ->
        do -- this is using the Maybe monad
          (s',f') <- f s
          fmap f' <$> x s'

{- `>>=` can be used to compose two parsers
   sequentially, so that the second parser
   operates on the leftovers of the first.
   If either parser fails, the composite
   parser also fails.

   For example, the following parser:

     parseDouble >>= \x ->
     keyword "+" >>
     parseDouble >>= \y ->
     return(x+y)

   or equivalently in do notation:

     do
       x <- parseDouble
       keyword "+"
       y <- parseDouble
       return(x+y)

   should return 3.0::Double when given
   the input string "1+2"
 -}
instance Monad Parser where
  return = pure
  Parser m >>= k =
    Parser $ \s ->
      do -- this is using the Maybe monad
        (s',a) <- m s
        let Parser m' = k a
        m' s'

{- A parser that always fails
 -}
abort :: Parser a
abort = Parser $ const Nothing

{- `try m` is a parser that fails if
   m is Nothing, and otherwise succeeds
   with result `m` without consuming any
   input.
 -}
try :: Maybe a -> Parser a
try Nothing  = abort
try (Just a) = return a

{- `keyword s` is a parser
   that consumes a string s,
   and fails if unable to do so.
 -}
keyword :: String -> Parser ()
keyword kw =
  Parser(\s ->
           let (xs,ys) = splitAt (length kw) s
           in
             if xs == kw then
               Just (ys,())
             else
               Nothing)

{- `parsePred f` is a parser
   that consumes and returns the
   longest prefix of the input
   such that every character satisfies
   `p`. Never fails.
 -}
parsePred :: (Char -> Bool) -> Parser String
parsePred f =
  Parser(\s ->
           let xs = takeWhile f s
           in
             Just (drop (length xs) s, xs))

{- `least f` consumes and returns the
   shortest prefix of the input that
   satisfies f.

   Fails if no prefix of the input
   satisfies f.
 -}
least :: (String -> Bool) -> Parser String
least f =
  Parser(\s -> do
            xs <- find f $ inits s
            return(drop (length xs) s, xs))

{- `orelse p1 p2` is a parser that
   behaves like p1, unless p1 fails.
   If p1 fails, it behaves instead like p2.
 -}
orelse :: Parser a -> Parser a -> Parser a
orelse (Parser p1) (Parser p2) =
  Parser $ \s ->
    case p1 s of
      Nothing -> p2 s
      res -> res

{- `parseWhile p` will repeatedly run
   the parser p until p fails,
   and return the list of all
   results from the successful
   runs in order.

   For example, we would expect:

   runParserPartial (parseWhile (keyword "bla")) "blablably"
   == Just ("bly",[(),()])

   Note that even if `p` fails,
   `parseWhile` should not fail.
 -}
parseWhile :: Parser a -> Parser [a]
parseWhile p =
  do
    x <- p
    xs <- parseWhile p
    return (x:xs)
  `orelse` return []

{- `first ps` behaves as the first parser
   in `ps` that does not fail. If they all
   fail, `first ps` fails too.
 -}
first :: [Parser a] -> Parser a
first [] = abort
first (p:ps) = p `orelse` (first ps)
    

{- peekChar is a parser that
   returns the first character
   of input without consuming it,
   failing if input is empty.

   For example, we'd expect

     runParserPartial peekChar "bla"
     == Just ("bla",'b')
 -}
peekChar :: Parser Char
peekChar = 
  Parser $ \s -> 
    if (length s) > 0 then
      Just (s, (head s))
    else
      Nothing

safePeekChar :: Parser String
safePeekChar = 
  Parser $ \s -> 
    if (length s) > 0 then
      Just (s, [head s])
    else
      Just (s, "")

safePeek2ndChar :: Parser String
safePeek2ndChar = 
  Parser $ \s -> 
    if (length s) > 1 then
      Just (s, [s !! 1])
    else
      Just (s, "")


{- parseChar is a parser that
   consumes (and returns) a single
   character, failing if there
   is nothing to consume.

   For example, we'd expect

     runParserPartial parseChar "bla"
     == Just ("la",'b')
 -}
parseChar :: Parser Char
parseChar = 
  Parser $ \s -> 
    if (length s) > 0 then
      Just ((tail s), (head s))
    else
      Nothing

{- A parser that consumes all leading
   whitespace (as determined by isSpace).
   Should always succeed.
 -}
whiteSpace :: Parser ()
whiteSpace = Parser $ \s -> do
  case runParserPartial (parsePred isSpace) s of
    Just (s', matches) -> Just (s', ())
    Nothing -> Just (s, ())

whiteSpace2 :: Parser ()
whiteSpace2 = (parsePred isSpace) >>= \_ -> return ()

{- parseBool either
   consumes "true" to produce True,
   consumes "false" to produce False,
   or fails if unable.
 -}
parseBool :: Parser Bool
parseBool = Parser $ \s ->
  if (take 4 s) == "true" then
    Just (drop 4 s, True)
  else if (take 5 s) == "false" then
    Just (drop 5 s, False)
  else
    Nothing

{- parsePositiveInt is a parser that parses a
   non-empty sequence of digits (0-9)
   into a number.
 -}
parsePositiveInt :: Parser Int
parsePositiveInt =
  do
    str <- parsePred isDigit
    if str == "" then
      Parser $ const Nothing
    else do
      let n = read str
      if n == 0 then
        Parser $ const Nothing
      else
        return $ read str

{- parseDouble is a parser that parses a number on the
   format:

     -<digits>.<digits>

   Where the minus sign and decimal point are optional.
   Should fail if the start of input does not match
   this format.

   The JSON file format also allows numbers with E notation.
   We do not support such numbers.
 -}
parseDouble :: Parser Double
parseDouble = do

  sign <- safeConsumeNextChar '-'

  pre <- parsePred isDigit
  when (pre == "") $ do
    abort

  dec <- safePeekChar
  afterDec <- safePeek2ndChar

  if (dec /= "." || afterDec == "" || (not $ isDigit $ head afterDec)) 
    then do
      let str = sign++pre
      return $ (read str)
    else do
      parseChar
      post <- parsePred isDigit
      let str = sign++pre++dec++post
      return $ (read str) 

safeConsumeNextChar :: Char -> Parser String
safeConsumeNextChar c = do
  x <- safePeekChar
  if x == [c]
    then do
      parseChar
      return [c]
    else do
      return ""

{- `parseString` is a parser that consumes a quoted
   string delimited by " quotes, and returns it.

   For example, we would expect:

     runParserPartial parseString "\"ab\"r"
     == runParserPartial parseString ['"','a','b','"','r']
     == Just ("r","ab")

   To add an additional complication, it's possible
   for the input string to contain escape sequences.
   Note in particular that escaped quotes do not end
   the string.
   For example:

    runParser parseString "\"a\\\"b\""
    ['"', 'a', '\"', 'b', '"']
    Just "a\"b"

   Hint: what does (readMaybe s)::Maybe String do?
         And how is that useful here?
         And how is what (readMaybe "[]")::Maybe String
         does less than optimally useful?
 -}

parseString :: Parser String
parseString = do
  str <- least readMaybeResult
  when ((head str) /= '"' || (last str) /= '"') abort
  try $ readMaybe str

readMaybeResult :: String -> Bool
readMaybeResult s =
  case readMaybe s :: Maybe String of
    Just a -> True
    Nothing -> False


-- doesnt work for edge cases eg \n
parseString2 :: Parser String
parseString2 = do
  keyword ['"']
  rest <- parseRestOfString
  return $ '"' : rest

parseRestOfString :: Parser String
parseRestOfString = do
  -- go char by char
  -- if curr char is \,
  -- add next char without caring what it is
  -- and dont add the curr \
  -- if we hit a " we end
  curr <- parseChar
  if (curr == '"') then do
    return ""
  else if (curr == '\\') then do
    next <- parseChar
    rest <- parseRestOfString
    return $ next : rest
  else do
    rest <- parseRestOfString
    return $ curr : rest

{- `parseList l r p` parses a
   comma-separated list that
   is delimited on the left by l,
   on the right by r,
   and where p is a parser for single
   elements of the list type.

   For example, we would expect:

     runParser (parseList '[' ']' parseDouble) "[1, 2]"
     == Just [1.0,2.0]

   Trailing commas, or omitted commas between elements,
   should yield failure.

   You should accept any amount of whitespace
   after l, before r, and before and after comma,
   including no whitespace. So we would expect
   the same result as above for the following call:

     runParser (parseList '[' ']' parseDouble) "[ 1  ,2 ]"

   The fact that we haven't hardcoded [ and ] as the
   delimiters will be handy later.
 -}
parseList :: Char -> Char -> Parser a -> Parser [a]
parseList l r elmP = do
  keyword [l]
  whiteSpace
  peek <- peekChar
  if (peek == r) then do
    parseChar
    return []
  else do
    parseRestOfList r elmP

parseRestOfList :: Char -> Parser a -> Parser [a]
parseRestOfList r elmP = do
  whiteSpace
  val <- elmP
  whiteSpace

  peek <- peekChar
  if (peek == r) then do
    parseChar
    return [val]
  else do
    keyword ","
    rest <- parseRestOfList r elmP
    return $ val : rest


{- `runParser s p` runs the parser p
   on input s.
   This should return Nothing if:
   - the parser fails, or
   - the parser succeeds, but has not
       consumed all input.
 -}
runParser :: Parser a -> String -> Maybe a
runParser (Parser p) s =
  case p s of
    Just ([],a) -> Just a
    _           -> Nothing

{- `runParserPartial s p` runs the parser p
   on input s.

   In the event that some of the input remains
   unconsumed, it is returned alongside the result.

   You should only use this for testing ---
   input that contains spare tokens at the end
   should be considered malformed, which is
   what `runParser` handles.
 -}
runParserPartial :: Parser a -> String -> Maybe(String,a)
runParserPartial (Parser p) s = p s

{- Now for our JSON parser.
   `JSON` is a datatype for representing
   JSON objects, which are key-value pairs
   mapping names to values.
 -}
data JSON = JSON [(String,Data)] deriving (Eq,Show)

{- Values in a JSON object can be of the following types:
   - a floating-point number
   - a string
   - a list of data objects, not necessarily of the same type
   - a boolean
   - null
   - another JSON object.
 -}
data Data =
  Number Double
  | String String
  | List [Data]
  | Bool Bool
  | Null
  | JSONData JSON deriving (Eq,Show)

{- This Arbitrary instance only generates keys in the
    range a..z.
   This is for readability, and should not be construed
   as a limitation on the type of keys allowed.

   The generator is deliberately biased towards
   smaller JSON objects.
 -}
instance Arbitrary JSON where
  arbitrary = JSON <$> listOf ((,) <$> key <*> arbitrary)
    where key = listOf $ elements ['a'..'z']
  shrink (JSON d) =
    JSON <$> shrinkList (const []) d

instance Arbitrary Data where
  shrink (JSONData j) = JSONData <$> shrink j
  shrink (List l) = List <$> shrink l
  shrink d = []  
  arbitrary = frequency [
                     (3,Number   <$> arbitrary),
                     (3,String   <$> arbitrary),
                     (1,List     <$> smaller arbitrary),
                     (3,Bool     <$> arbitrary),
                     (1,JSONData <$> smaller arbitrary),
                     (1,pure Null)] where
    smaller g = sized (\n -> resize (n `div` 4) g)

{- A parser for JSON objects.

   JSON objects are {}-delimited,
   comma-separated lists of key-value
   pairs.

   A key-value pair is a "-delimited string
   and a data value, separated by a :

   Allow any amount of whitespace
   before or after the elements

     { } : ,

   Hints:
    Because JSON objects can themselves be data, this
    function needs to be mutually recursive with parseData.
 -}
parseJSON :: Parser JSON
parseJSON = do
  whiteSpace
  l <- parseList '{' '}' parseJSONElm
  return $ JSON l
  -- open { }
  -- parseList of JSON elements
  -- will take care of the , between elm


parseJSONElm :: Parser (String,Data)
parseJSONElm = do
  -- parse string key
  whiteSpace
  key <- parseString
  -- parse `:`
  whiteSpace
  keyword ":"
  whiteSpace
  -- parse value
  value <- parseData
  whiteSpace
  return (key, value)

{- A parser for JSON data values.

   Hints:
    You've already done most of the work for this in the
    previous functions, it's just a matter of composing
    them.

    Because JSON objects can themselves be data, this
    function needs to be mutually recursive with parseJSON.
 -}
parseData :: Parser Data
parseData = first [
  fmap Number parseDouble,
  fmap String parseString,
  fmap List (parseList '[' ']' parseData),
  fmap Bool parseBool,
  fmap JSONData parseJSON,
  parseNull
  ]

parseNull :: Parser Data
parseNull = do
  keyword "null"
  return Null


{- Time strings are represented in the following format:

     YYYY-MM-DD HH-MM-SS

   where H is in 24h format. This is a string-to-time
   function that accepts the above format.
 -}
toTime :: String -> Maybe UTCTime
toTime = parseTimeM True defaultTimeLocale "%F %X"



{- A quiz submission consists of:

  - A session (something like 23T2)
  - Name of the quiz (something like quiz02)
  - Name of the student (something like "Jean-Baptiste Bernadotte")
  - A list of answers.
    For example, a student who answer option 4 on question 1,
    and options 1,3 on question 2,
    would have [[4],[1,3]]
  - A submission time.
 -}
data Submission =
  Submission { session :: String,
               quizName :: String,
               student :: String,
               answers :: [[Int]],
               time :: UTCTime
             } deriving (Eq,Show)

instance Arbitrary Submission where
  arbitrary =
    Submission <$>
    arbitrary <*>
    arbitrary <*>
    arbitrary <*>
    arbitraryAnswers <*>
    arbitraryTime where arbitraryAnswers = map (map getPositive) <$> arbitrary

-- Likes generating years in the far future.
arbitraryTime :: Gen UTCTime
arbitraryTime =
  do
    Large d <- arbitrary
    t <- choose (0,86400)
    return $ UTCTime (toEnum $ abs d) (secondsToDiffTime t)
          

{- These utility functions will be handy I promise -}
getString :: Data -> Maybe String
getString (String s) = return s
getString _ = Nothing

getList :: Data -> Maybe [Data]
getList (List xs) = return xs
getList _ = Nothing

getNumber :: Data -> Maybe Double
getNumber (Number n) = return n
getNumber _ = Nothing

getJSON :: Data -> Maybe JSON
getJSON (JSONData j) = return j
getJSON _ = Nothing

{- This function should convert a JSON object
   to a `Submission`.
   This should fail if:
   - Either of the keys
       session, quiz_name, student, answers,time
     are absent from the JSON object
   - Either of the above are present, but
     do not have the expected type.
     All fields except `answers` are expected to
     hold string values; `answers` should hold
     a list of lists of numbers.
   - The `time` key is present, and holds
     a string, but this string is not
     recognised as a valid time by
     `toTime`.
   - The `answers` contain numbers that are
     not whole, or not positive.

   You should *not* fail if the JSON object
   contains more keys than the above.
   Politely ignore such keys.

   If either of the above keys have duplicate
   entries in the JSON object, you're free
   to do anything you want.
   Duplicates of other keys should be ignored.
 -}
toSubmission :: JSON -> Maybe Submission
-- JSON = JSON [(String, Data)]
-- toSubmission = error "hi"
--        session, quiz_name, student, answers,time
toSubmission json = do
  session <- (getDataByKey "session" json) >>= getString
  quiz_name <- (getDataByKey "quiz_name" json) >>= getString
  student <- (getDataByKey "student" json) >>= getString
  time <- (getDataByKey "time" json) >>= getString >>= toTime

  answersData <- (getDataByKey "answers" json) >>= getList -- [Data]
  answersListData <- convertL1ToL2 answersData -- [[Data]]
  answers <- convertL2toAnswers answersListData -- [[Int]]


  return Submission {
    session = session,
    quizName = quiz_name,
    student = student,
    answers = answers,
    time = time
  }

getDataByKey :: String -> JSON -> Maybe Data
getDataByKey key (JSON json) = do
  (k, v) <- find (\(k, v) -> k == key) json
  return v

convertToList :: (a -> Maybe b) -> [a] -> Maybe [b]
convertToList _ [] = Just []
convertToList f (x:xs) = do
  curr <- f x
  rest <- convertToList f xs
  return $ curr : rest

convertL1ToL2 :: [Data] -> Maybe [[Data]]
convertL1ToL2 xs = convertToList getList xs

-- convertL1ToL2 :: [Data] -> Maybe [[Data]]
-- convertL1ToL2 [] = Just []
-- convertL1ToL2 (x:xs) = do 
--   curr <- getList x
--   rest <- convertL1ToL2 xs
--   return $ curr : rest

convertL2toAnswers :: [[Data]] -> Maybe [[Int]]
convertL2toAnswers xs = convertToList convertL2toAnswersAux xs
-- convertL2toAnswers [] = Just []
-- convertL2toAnswers (l:ls) = do 
--   curr <- convertL2toAnswersAux l
--   rest <- convertL2toAnswers ls
--   return $ curr : rest

convertL2toAnswersAux :: [Data] -> Maybe [Int]
convertL2toAnswersAux xs = convertToList (\x -> getNumber x >>= convertToPositiveInt) xs 

-- convertL2toAnswersAux [] = Just []
-- convertL2toAnswersAux (x:xs) = do 
--   curr <- getNumber x >>= convertToPositiveInt
--   rest <- convertL2toAnswersAux xs
--   return $ curr : rest

isWholeNumber :: Double -> Bool
isWholeNumber x = snd (properFraction x) == 0

convertToPositiveInt :: Double -> Maybe Int
convertToPositiveInt dub
  | isWholeNumber dub && dub > 0 = Just (round dub)
  | otherwise = Nothing

{- This function should convert a JSON object
   to a key-value store where the values are
   of type `Submission` instead of JSON objects.

   Should fail if the JSON object holds one or
   more values that are not valid submissions.
 -}
 
toSubmissions :: JSON -> Maybe [(String, Submission)]
toSubmissions (JSON json) = do
  subPairs <- convertElmsToSubPairs json
  return subPairs

-- intentionally not condensed, would get more confusing
convertElmsToSubPairs :: [(String, Data)] -> Maybe [(String, Submission)]
convertElmsToSubPairs [] = Just []
convertElmsToSubPairs (x:xs) = do  -- [(String,Data)]
  sj <- convertElmToSubPair x -- [(String, JSON)]
  ss <- handleSubmissionPair sj -- [(String, Submission)]
  rest <- convertElmsToSubPairs xs
  return $ ss : rest

convertElmToSubPair :: (String, Data) -> Maybe (String, JSON)
convertElmToSubPair (s, d) = do
  j <- getJSON d
  return (s, j)

handleSubmissionPair :: (String, JSON) -> Maybe (String, Submission)
handleSubmissionPair (key, json) = do
  sub <- toSubmission json
  return $ (key, sub)


{- There are two kinds of questions:
   - multiple-choice, represented by CheckBox
   - single-choice, represented by Radio
 -}
data QuestionType = Radio | CheckBox deriving (Eq,Show)

instance Arbitrary QuestionType where arbitrary = elements [Radio,CheckBox]

{- A question has:
   - a question number
   - a question type
   - a list of correct answers
 -}
data Question =
  Question { number  :: Int,
             qtype   :: QuestionType,
             correct :: [Int]
           } deriving (Eq,Show)

{- A quiz comprises a deadline and a list of questions.
 -}
data Quiz =
  Quiz { deadline :: UTCTime,
         questions :: [Question]
       } deriving (Eq,Show)

instance Arbitrary Question where
  arbitrary = do
    Positive n <- arbitrary
    qtype <- arbitrary
    correct <- listOf1 (getPositive <$> arbitrary)
    return $ Question n qtype correct

nubBy :: Eq b => (a -> b) -> [a] -> [a]
nubBy f [] = []
nubBy f (x:xs) = x:nubBy f (filter ((/= f x) . f) xs)

{- This generator is set up to generate quizzes with distinct
   question numbers starting from 1.

   This partially overrides the behaviour of the Question
   generator.
 -}
instance Arbitrary Quiz where
  arbitrary = Quiz <$> arbitraryTime <*> arbitraryQuestions
    where
      arbitraryQuestions = do
        qs <- arbitrary
        return $ map (\(n,q) -> Question n (qtype q) (correct q)) $ zip [1..] qs

{- `parseQuiz` is a parser that
   reads a quiz from an input representing
   a key. A key is a file such that:

   - The first line is a date, of the form
     accepted by toTime
   - The subsequent lines represent questions,
     and have the form

       n|type|correct

     Where `n` is a question number,
     `type` is either a checkbox or a radio button,
     and `correct` is a comma-separated list
     of answers with no delimiters.
     The answer must be a a list of non-empty
     positive integers denoting
     the correct alternatives.

   Malformed input should be rejected.
   Questions must start from 1, be consecutive,
   be positive integers,
   and be free of duplicates question numbers;
   reject any input that does not conform.
   Answers must be positive integers,
   and there must be at least one answer.
 -}
parseQuiz :: Parser Quiz
-- -- parse time
-- -- parse questions, making sure they're matching their required number
parseQuiz = do
  firstLine <- parsePred (\c -> c /= '\n')
  time <- try $ toTime firstLine
  parseChar -- consume newline

  questions <- parseQuestions 1
  -- condition 1: consecutive qs starting from 1
  questions' <- if (checkQuestions questions 1) then do
    return questions
  else
    error ("failed checkQuestions\n" ++ (show questions))
    abort

  return Quiz {
    deadline = time,
    questions = questions'
  }

-- parseQuestions2 :: Parser [Question]
-- parseQuestions2 = parseWhile parseQuestion

parseQuestions :: Int -> Parser [Question]
parseQuestions number = do
  peek <- safePeekChar
  if (peek == "") then do -- EOF
    return []
  else do
    q <- parseQuestion number
    rest <- parseQuestions (number+1) `orelse` (return [])
    return $ q : rest

parseQuestion :: Int -> Parser Question
parseQuestion number = do
  peek <- peekChar
  when ([peek] /= (show number)) abort

  qn <- parsePositiveInt
  keyword "|"

  -- condition 2: radio | checkbox
  qtype <- ((keyword "radio") >>= \_ -> return Radio) 
    `orelse` ((keyword "checkbox") >>= \_ -> return CheckBox)
  keyword "|"

  -- condition 3: at least 1 correct answer
  correct <- parseCorrect
  correct' <- if length correct > 0
    then return correct
    else abort 

  return Question {
    number = qn,
    qtype = qtype,
    correct = correct'
  }


parseCorrect :: Parser [Int]
parseCorrect = do
  ans <- parsePositiveInt
  peek <- safePeekChar
  peek2 <- safePeek2ndChar
  if (peek == "") then do -- EOF
    return [ans]
  else if (peek == "\n") then do -- end of line
    parseChar
    return [ans]
  else if (peek == "," && peek2 == " ") then do -- more answers
    parseChar -- consume ','
    parseChar -- consume ' ', assuming exactly 1 space
    rest <- parseCorrect
    return $ ans : rest
  else
    error ("saw something unexpected: '" ++ peek ++ "'")
    abort


checkQuestions :: [Question] -> Int -> Bool
checkQuestions [] _ = True
checkQuestions (q:qs) n = ((number q) == n) && checkQuestions qs (n+1)

{- And now for the business logic!

   `markQuestion q as` should assign marks
   to the answers `as` given to question `q`
   as follows:

   - If the question is single-choice,
     give 1 mark if the student supplied
     exactly one *unique* answer, which is also
     correct.
   - If the question is multiple-choice,
     give marks according to the
     following formula:

       max(0,(right - wrong)/correct)

     Where `right` is the number of correct
     answers in `as`, `wrong` is the number
     of incorrect answers in `as`,
     and `correct` is the number of correct
     answers in the answer sheet.

     Allow for the possibility that either
     the given set of answers, or the
     answer sheet, may contain duplicates.
     Duplicates should be ignored
     for purposes of the above tally.
 -}
markQuestion :: Question -> [Int] -> Double

markQuestion q as = 
  case qtype q of
    Radio -> markRadio q as
    CheckBox -> markCheckBox q as  
  
markRadio :: Question -> [Int] -> Double
markRadio q as
  | oneAnswer && correctAnswer = 1
  | otherwise = 0
  where
    uniqAs = nub as
    oneAnswer = (length uniqAs == 1)
    correctAnswer = elem (head uniqAs) (correct q)

markCheckBox :: Question -> [Int] -> Double
markCheckBox q as = max 0 ((right - wrong) / lenCorrAns)
  where
    corrAns = nub (correct q)
    studAns = nub as
    lenCorrAns = fromIntegral $ length corrAns

    right = fromIntegral $ length $ filter (\a -> (elem a corrAns)) studAns
    wrong = fromIntegral $ length $ filter (\a -> not (elem a corrAns)) studAns


qr = Question {number = 1, qtype = Radio, correct = [1,2,3]}
qc = Question {number = 1, qtype = CheckBox, correct = [1,2,3]}
as1 :: [Int] = [1]
as2 :: [Int] = [1,2]

{- The mark assigned to a quiz submission is:
   - 0 if submitted after the deadline.
   - the sum of the marks for each question,
     if submitted at or before the deadline.
 -}
markSubmission :: Quiz -> Submission -> Double
-- markSubmission = error "TODO: implement marker"
markSubmission quiz submission = 
  if deadl > subTime 
    then markQuestions qs ass   -- valid submission
    else 0                      -- late
  where
    deadl = deadline quiz
    subTime = time submission
    qs = questions quiz
    ass = answers submission

markQuestions :: [Question] -> [[Int]] -> Double
markQuestions _ [] = 0
markQuestions [] _ = 0
markQuestions (q:qs) (as:ass) = (markQuestion q as) + (markQuestions qs ass)

{- `marker quizStr submissionsStr`
   combines the parsers and business logic as follows:

   If `quizStr` can be parsed to a quiz,
   and if `submissionsStr` can be parsed to
   a `[(String,Submission)]` using
   parseJSON and toResults,
   calculate quiz marks for each student
   and present them in the form of an
   sms update file.
   update files look like this:

     z1234567|quiz01|3
     z2345678|quiz01|7

   with each line being a | separated tuple
   of student ID, quiz name, and marks.
   No extra spaces beyond the newlines
   at the end of each line (including the
   last line).

   The order of the lines is not important.
 -}
marker :: String -> String -> Maybe String
-- marker = error "TODO: implement marker"
marker quizStr submissionsStr = do
  quiz <- runParser parseQuiz quizStr
  submissions <- (runParser parseJSON submissionsStr) >>= toSubmissions
  fmap trim (makeUpdateFile quiz submissions)

makeUpdateFile :: Quiz -> [(String, Submission)] -> Maybe String
makeUpdateFile quiz [] = return ""
makeUpdateFile quiz ((zid, sub):submissions) = do
  let mark = markSubmission quiz sub
  let quizNum = quizName sub
  rest <- makeUpdateFile quiz submissions
  return $ (zid ++ "|" ++ quizNum ++ "|" ++ (show mark) ++ "\n" ) ++ rest

-- source: 
trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

  -- zid from submission
  -- quizNum from submission
  -- result from markSubmission

{- Use this to read a quiz key and submissions
   file from the file system, and print the
   updated file to stdOut.

   Note that FilePath is a type synonym for String.
 -}
runMarker :: FilePath -> FilePath -> IO ()
runMarker quizFile submissionsFile = do
  quiz <- readFile quizFile
  submissions <- readFile submissionsFile
  case marker quiz submissions of
    Nothing -> putStrLn "Something went wrong. But this error message is not helpful!"
    Just output -> putStrLn output


raghavTestMarker :: IO ()
raghavTestMarker = do
    quiz <- readFile "quiz1.txt"
    submissions <- readFile "submissions1.txt"
    upload <- readFile "upload1.txt"
    output <- try2 $ marker quiz submissions
    putStrLn $ show (upload == output)

try2 :: Maybe a -> IO a
try2 (Just a) = return a
try2 Nothing  = error "Encountered a Nothing value."



-- Unit tests (stolen from forum)
-- Task 2
prop_parseDataWorks :: Data -> Bool
prop_parseDataWorks d = runParser parseData (dataToStr d) == Just d

prop_parseJSONWorks :: JSON -> Bool
prop_parseJSONWorks j = runParser parseJSON (jsonToStr j) == Just j

-- Task 3
prop_toSubmissionWorks :: Submission -> Bool
prop_toSubmissionWorks s = toSubmission (submissionToJSON s) == Just s

prop_toSubmissionsWorks :: [(String, Submission)] -> Bool
prop_toSubmissionsWorks ss = toSubmissions (submissionsToJSON ss) == Just ss

-- Task 4
prop_parseQuizWorks :: Quiz -> Property
prop_parseQuizWorks q = not (null (questions q)) ==>
  runParser parseQuiz (quizToStr q) == Just q

(Just t) = (toTime "1858-11-19 13:27:44")
quiz = Quiz {deadline = t, questions = [Question {number = 1, qtype = CheckBox, correct = [2,2]}]}

-- Task 5 (may be very slow)
prop_markerSucceeds :: Quiz -> [(String, Submission)] -> Property
prop_markerSucceeds q ss = not (null (questions q)) ==>
  isJust (marker (quizToStr q) (jsonToStr (submissionsToJSON ss)))

-- Helper functions for converting from parsed data to unparsed format
jsonToStr :: JSON -> String
jsonToStr (JSON j) = "{" ++ intercalate ", " (map keyValuePairToStr j) ++ "}"

keyValuePairToStr :: (String, Data) -> String
keyValuePairToStr (k, v) = show k ++ ": " ++ dataToStr v

dataToStr :: Data -> String
dataToStr (Number n) = showFFloat Nothing n ""
dataToStr (String s) = show s
dataToStr (List l) = "[" ++ intercalate ", " (map dataToStr l) ++ "]"
dataToStr (Bool b) = if b then "true" else "false"
dataToStr Null = "null"
dataToStr (JSONData j) = jsonToStr j

submissionToJSON :: Submission -> JSON
submissionToJSON (Submission se q st a t) = JSON
  [("session", String se),
   ("quiz_name", String q),
   ("student", String st),
   ("answers", List $ map (List . map (Number . fromIntegral)) a),
   ("time", String $ timeToStr t)]

submissionsToJSON :: [(String, Submission)] -> JSON
submissionsToJSON ss = JSON $ map (second (JSONData . submissionToJSON)) ss

timeToStr :: UTCTime -> String
timeToStr = unwords . take 2 . words . show -- drops tz

quizToStr :: Quiz -> String
quizToStr (Quiz d qs) =
  unlines $ timeToStr d : map questionToStr qs

questionToStr :: Question -> String
questionToStr (Question n t cs) =
  show n ++ "|" ++
  (if t == Radio then "radio" else "checkbox") ++ "|" ++
  intercalate ", " (map show cs)