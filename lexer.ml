open TokenTypes

(* Part 1: Lexer - IMPLEMENT YOUR CODE BELOW *)

let tokenize input = 
    let rec tokenize_helper pos = 
        if pos >= String.length input then
            []
        else if (Str.string_match (Str.regexp "<>") input pos) then
            Tok_NotEqual::(tokenize_helper (pos + 2))
        else if (Str.string_match (Str.regexp "=") input pos) then
            Tok_Equal::(tokenize_helper (pos + 1))
        else if (Str.string_match (Str.regexp "->") input pos) then
            Tok_Arrow::(tokenize_helper (pos + 2))
        else if (Str.string_match (Str.regexp ">=") input pos) then
            Tok_GreaterEqual::(tokenize_helper (pos + 2))
        else if (Str.string_match (Str.regexp "<=") input pos) then
            Tok_LessEqual::(tokenize_helper (pos + 2))
        else if (Str.string_match (Str.regexp ">") input pos) then
            Tok_Greater::(tokenize_helper (pos + 1))
        else if (Str.string_match (Str.regexp "<") input pos) then
            Tok_Less::(tokenize_helper (pos + 1))
        else if (Str.string_match (Str.regexp "||") input pos) then
            Tok_Or::(tokenize_helper (pos + 2))
        else if (Str.string_match (Str.regexp "&&") input pos) then
            Tok_And::(tokenize_helper (pos + 2))
        else if (Str.string_match (Str.regexp "\\+") input pos) then
            Tok_Add::(tokenize_helper (pos + 1))
        else if (Str.string_match (Str.regexp "-") input pos) then
            Tok_Sub::(tokenize_helper (pos + 1))
        else if (Str.string_match (Str.regexp "\\*") input pos) then
            Tok_Mult::(tokenize_helper (pos + 1))
        else if (Str.string_match (Str.regexp "\\/") input pos) then
            Tok_Div::(tokenize_helper (pos + 1))
        else if (Str.string_match (Str.regexp "\\^") input pos) then
            Tok_Concat::(tokenize_helper (pos + 1))
        else if (Str.string_match (Str.regexp "(-[0-9]+)") input pos) then
            let int_token = Str.matched_string input in 
            let new_token = int_of_string (String.sub int_token 1 (String.length int_token - 2)) in
                Tok_Int new_token::(tokenize_helper (pos + (String.length int_token)))
        else if (Str.string_match (Str.regexp "[0-9]+") input pos)  then
            let int_token = Str.matched_string input in 
                Tok_Int (int_of_string int_token)::(tokenize_helper (pos + (String.length int_token)))
        else if (Str.string_match (Str.regexp ";;") input pos) then
            Tok_DoubleSemi::(tokenize_helper (pos + 2))
        else if (Str.string_match (Str.regexp "(") input pos) then
            Tok_LParen::(tokenize_helper (pos + 1))
        else if (Str.string_match (Str.regexp ")") input pos) then
            Tok_RParen::(tokenize_helper (pos + 1))
        else if (Str.string_match (Str.regexp "\"[^\"]*\"") input pos) then
            let str_token = Str.matched_string input in
            let sanitze_str = String.sub str_token 1 ((String.length str_token) - 2) in
                Tok_String sanitze_str::(tokenize_helper (pos + (String.length str_token)))
        else if (Str.string_match (Str.regexp "[a-zA-Z][a-zA-Z0-9]*") input pos) then
            let token = Str.matched_string input in
                if (token = "not") then
                    Tok_Not::(tokenize_helper (pos + 3))
                else if (token = "if") then
                    Tok_If::(tokenize_helper (pos + 2))
                else if (token = "then") then
                    Tok_Then::(tokenize_helper (pos + 4))
                else if (token = "else") then
                    Tok_Else::(tokenize_helper (pos + 4))
                else if (token = "let") then
                    Tok_Let::(tokenize_helper (pos + 3))
                else if (token = "def") then
                    Tok_Def::(tokenize_helper (pos + 3))
                else if (token = "in") then
                    Tok_In::(tokenize_helper (pos + 2))
                else if (token = "rec") then
                    Tok_Rec::(tokenize_helper (pos + 3))
                else if (token = "fun") then
                    Tok_Fun::(tokenize_helper (pos + 3))
                else if (token = "true") then
                    Tok_Bool true::(tokenize_helper (pos + 4))
                else if (token = "false") then
                    Tok_Bool false::(tokenize_helper (pos + 5)) 
                else
                    Tok_ID token::(tokenize_helper (pos + (String.length token)))
        else if (Str.string_match (Str.regexp " ") input pos) then
            tokenize_helper (pos + 1)
        else
            raise (InvalidInputException "Invalid Token")
        in
        tokenize_helper 0



