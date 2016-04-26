// See the file alphabet-cipher.md for detailed information.

type Message = string
type Keyword = string


open System


// Helper Functions (rather generic)

/// <summary>
/// Convert a character to integer.
/// </summary>
let charToInt (ch: char) =
    Convert.ToInt32(ch)


/// <summary>
/// Convert an integer to a character.
/// </summary>
let intToChar (num: int) =
    Convert.ToChar(num)


/// <summary>
/// Lower-case a character.
/// </summary>
let charToLower (ch: char) =
    ch.ToString().ToLower().[0] |> char


/// <summary>
/// Get the zero-based numeric position of a character
/// in the alphabet 'a'..'z'.
/// </summary>
let getLetterPosition (ch: char) =

    let c = charToLower ch

    // character - 'a' = zero-based index
    (-) <| charToInt c <| charToInt 'a'


/// <summary>
/// Get the character in the alphabet 'a'..'z' denoted by
/// zero-based numeric position.
/// </summary>
let letterPositionToChar =

    // index + value of 'a' then casted to char = character
    let addA = (+) <| charToInt 'a'

    addA >> intToChar


/// <summary>
/// Sanitize string to 'a'..'z' lower-case characters only.
/// </summary>
let sanitizeString (str: string) =

    let lowerBound = charToInt 'a'
    let upperBound = charToInt 'z'

    str.ToLower().ToCharArray()
    |> Array.filter (fun ch ->
                            let intValue = charToInt ch
                            intValue >= lowerBound && intValue <= upperBound
                        )
    |> Array.map string
    |> Array.fold (+) ""


// Dictionary-Related Helper Functions

/// <summary>
/// Generates the character sequence 'a' .. 'z'
/// </summary>
let getAlphabet () =

    [ 0..25 ]
    |> List.map letterPositionToChar
    |> List.toSeq


/// <summary>
/// Constructs a row of alphabet characters 'a'..'z' left-shifted by
/// the positional value of a character thus
/// argument 'c' will result in the list [ 'c'; 'd'; 'e'; ... 'z'; 'a'; 'b' ].
/// </summary>
let getRowForLetter (letter: char) =

    let pos = getLetterPosition letter

    let remainder =
        getAlphabet ()
        |> Seq.take pos
        |> Seq.toList

    let beginning =
        getAlphabet ()
        |> Seq.skip pos
        |> Seq.toList

    beginning @ remainder


/// <summary>
/// Create a map of shifted character rows indexed and shifted by
/// the characters 'a'..'z'.
/// </summary>
let createDictionary () =

    [ 0..25 ]
    |> List.map (fun pos ->
                        let ch = pos |> letterPositionToChar
                        let row = ch |> getRowForLetter
                        (ch, row)
                    )
    |> List.toSeq
    |> Map


/// <summary>
/// Infinitely repeat a keyword string in order to support
/// List.zip with arbitrary message sizes.
/// </summary>
let getInfiniteKeywordSequence (key: Keyword) =

    Seq.initInfinite id
    |> Seq.map (fun pos ->
                        let safePos = pos % key.Length
                        key.[safePos]
                    )


/// <summary>
/// Lookup the encoded character based on a key and a plain-text character
/// in the dictionary.
/// </summary>
// The pairs in the dictionary are indexed by the possible letters in the 
// message (keys).
// Each value is a list of characters that can be indexed by
// the numerical position of the key character in the alphabet.
let encodeLookup (keyChar: char) (messageChar: char) =

    let keyPos = getLetterPosition keyChar

    let alphaMap = createDictionary ()

    alphaMap.[messageChar].[keyPos]


/// <summary>
/// Lookup the plain-text character based on a key and a cipher character
/// in the dictionary.
/// </summary>
// To reverse the encoding we must slice the dictionary values to the column
// that corresponds to the key character position in the alphabet.
// Then we must find the value that is identical to the character of the
// cipher. The matching key to that value is the decoded letter
// of the message.
let decodeLookup (keyChar: char) (cipherChar: char) =

    let keyPos = getLetterPosition keyChar

    createDictionary ()
    |> Map.filter (fun _ cival -> cival.[keyPos] = cipherChar)
    |> Map.fold (fun state mkey _ -> (+) state <| string mkey) ""
    |> char


/// <summary>
/// Lookup the key character based on a cipher and a plain-text character
/// in the dictionary.
/// </summary>
let decipherLookup (cipherChar: char) (messageChar: char) =

    let alphaDict = createDictionary ()

    alphaDict.[messageChar]
    |> List.findIndex (fun ch -> ch = cipherChar)
    |> letterPositionToChar


/// <summary>
/// Helper function to generate a repeated character list from the
/// Keyword (string) based on Message (string) length.
/// </summary>
let getKeyList (key: Keyword) (message: Message) =

    key
    |> getInfiniteKeywordSequence
    |> Seq.take message.Length
    |> Seq.toList


/// <summary>
/// Convert a Message (string) to a list of characters.
/// </summary>
let getMessageList (message: Message) =

    message.ToCharArray()
    |> Array.toList


/// <summary>
/// Helper function to generalize the encoding/decoding process by passing
/// the operation as a function argument.
/// </summary>
let performCharacterConversion (operation: char -> char -> char) (key: Keyword) (message: Message) : Message =

    let sanitizedKey = sanitizeString key
    let sanitizedMessage = sanitizeString message

    let keyList = getKeyList sanitizedKey sanitizedMessage
    let messageList = getMessageList sanitizedMessage

    List.zip keyList messageList
    |> List.map (fun (kchar, mchar) -> operation kchar mchar)
    |> List.map string
    |> List.fold (+) ""


// Debugging Output
printfn ""
printfn "Dictionary:"
printfn "%A" <| createDictionary ()
printfn ""


// Kata Task Functions

let encode (key:Keyword) (message:Message) : Message =

    performCharacterConversion encodeLookup key message


let decode (key:Keyword) (message:Message) : Message =

    performCharacterConversion decodeLookup key message


let decipher (cipher:Message) (message:Message) : Keyword =

    let sanitizedCipher = sanitizeString cipher
    let sanitizedMessage = sanitizeString message

    let cipherList = getMessageList sanitizedCipher
    let messageList = getMessageList sanitizedMessage

    let expandedKey =
        List.zip cipherList messageList
        |> List.map (fun (c, m) -> decipherLookup c m)
        |> List.map string
        |> List.fold (+) ""

    // Well, how to find and eliminate character repetitions???
    // Key is currently repeated and truncated at message length!

    [1 .. expandedKey.Length]
    |> List.toSeq
    |> Seq.map (fun i -> expandedKey.Substring(0, i))
    |> Seq.filter (fun str -> cipher = encode str message)
    |> Seq.take 1
    |> Seq.fold (+) ""



// Kata Tests

#r @"../packages/Unquote/lib/net45/Unquote.dll"
open Swensen.Unquote

let tests () =

    // verify encoding
    test <@ encode "vigilance" "meetmeontuesdayeveningatseven" = "hmkbxebpxpmyllyrxiiqtoltfgzzv" @>
    test <@ encode "scones" "meetmebythetree" = "egsgqwtahuiljgs" @>

    // verify decoding
    test <@ decode "vigilance" "hmkbxebpxpmyllyrxiiqtoltfgzzv" = "meetmeontuesdayeveningatseven" @>
    test <@ decode "scones" "egsgqwtahuiljgs" = "meetmebythetree" @>

    // verify decyphering
    test <@ decipher "opkyfipmfmwcvqoklyhxywgeecpvhelzg" "thequickbrownfoxjumpsoveralazydog" = "vigilance" @>
    test <@ decipher "hcqxqqtqljmlzhwiivgbsapaiwcenmyu" "packmyboxwithfivedozenliquorjugs" = "scones" @>

// run the tests
tests ()
