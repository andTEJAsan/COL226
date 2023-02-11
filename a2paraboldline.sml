fun readFile file =
    let
        val inStream = TextIO.openIn file
        val contents = TextIO.inputAll inStream
    in
        TextIO.closeIn inStream;
        contents ^""
    end

fun writeFile (filename: string, contents: string) =
   let
      val file = TextIO.openOut filename
   in
      TextIO.output(file, contents);
      TextIO.closeOut(file)
   end



(*datatype regexp  = Char of char | Plus of regexp*regexp | Times of regexp*regexp | Star of regexp | One | Zero;*)
exception parseError
exception stackError
exception unmatchedError
(*fun matcher(s: string,r:regexp) = 
    case r of 
    Zero => false
    
    | One => let val x = (s = "") in if(x) then false else true end
    | Char c => let val x = (String.size(s) = 1) 
                    val y = (String.str(c) = s)
                    in x andalso y end
    | Star(c) => let val x = (s="") in if (x) then true else matcher(substring(s,0,1),c) andalso matcher(substring(s,1,size(s)-1),r) end
    | Plus(x,y) => matcher(s,x) orelse matcher(s,y)
    | Times(x,y) => let fun matchi (i : int ) : bool = if i > size(s) then false else if (matcher(substring(s,0,i),x) andalso matcher(substring(s,i,size(s)-i),y)) then true else matchi(i+1)

                    in matchi(0) end;

val italictextregex =Times(Char(#"*"),Times(One,Char(#"*"))) 
(*fun textparser(s:string) : string*string = if s = "" then ("","") else
                                            let val x = substring(s,0,1) 
                                            in 
                                            if(x = "*") then
                                            let val y =
                                            let fun italicextract(i:int) = 
                                            if i > size(s) then size(s)+1 else if(matcher(substring(s,0,i),italictextregex)) then i else italicextract(i+1)
                                                            in italicextract(0) end
                                            in if (y > size(s)) then ("",s) else ("<i>"^substring(s,1,y-2)^"</i>",substring(s,y,size(s)-y)) end
                                            else
                                            (substring(s,0,1),substring(s,1,size(s)-1))
                                            
                                            end
fun puttogether(s:string) = let 
                            val (x,y) = textparser(s)
                            in 
                                if y = "" then y else x ^ puttogether(y)
                            end


val a = readFile("b.txt")
val x = size(a)
*)
val newline : regexp = Char(#"\n")
val li : regexp = Char(#"-")
val hrregex = Times(Times(Times(Times(li,li),li),Star(li)),newline)
val head : regexp = Char(#"#")
val h1regex = Times(Times(head,One),newline)*)

fun headfind(s:string, i : int, set : int) = (*returns the number of # in headeing *)  
                                                        if i = 6 then 6 
                                                        else if (set +i)<size(s) then ( if substring(s, set+i,1) <> "#" then i else headfind(s,i+1,set) ) else i 

fun charfind(s: string, i: int, c: char) = (*returns the first occurence of a character in string, starting checking from i*)
                                     if i = size(s) then raise unmatchedError  else if substring(s,i,1) = String.str(c) then i else charfind(s,i+1,c) 
(*val italictextregex =Times(Char(#"*"),Times(One,Char(#"*"))) 
val boldregex = Times(Times(Char(#"*"),italictextregex),Char(#"*"))
val linknamereg = Times(Times(Char(#"["),One),Char(#"]"))
val linkrefreg = Times(Times(Char(#"("),One),Char(#")"))
val underlineregex =Times(Char(#"_"),Times(One,Char(#"_")))
val linkreg = Times(linknamereg,linkrefreg)

fun lineparse(s:string) :(string*string)  = (*supports italics,bold,underlining and links*)
                                            if s = "" then ("","")
                                            else let fun matchi(i: int) = 
                                            if i = size(s) then raise parseError
                                            else if matcher(substring(s,0,i+1),italictextregex) then
                                            let val x = substring(s,1,i-1) in ("<i>"^x^"</i>",substring(s,i+1,size(s)-i-1)) end
                                            else if matcher(substring(s,0,i+1),boldregex) then
                                            let val x = substring(s,2,i-3) in ("<b>"^x^"</b>",substring(s,i+1,size(s)-i-1)) end
                                            else if matcher(substring(s,0,i+1),underlineregex) then
                                            let val x = substring(s,1,i-1) in ("<u>"^x^"</u>",substring(s,i+1,size(s)-i-1)) end
                                            else if substring(s,0,1) <> "*" andalso substring(s,0,1) <> "[" andalso substring(s,0,1) <> "_" then (substring(s,0,i+1),substring(s,i+1,size(s)-i-1))
                                            else matchi(i+1)
                                              
                                            
                                            in matchi(0) end


fun lineparsetotal(s: string) = 
let val (x,y) = lineparse(s) in x^lineparsetotal(y) end         *)                                   
exception Stupid
fun lineparse1(s: string) =let  fun lineparse2(i:int,stack : char list,text) = 
(* there will be four states namely n -> normal,u ->underline,b->bold,i->italic ,o ->outside mode*)

                        if i=size(s) andalso length(stack) <>1  then raise stackError  else if i >= size(s) then text else

                        let
                            val (nextpos,stackn,ntext) = 
                                            if (hd stack = #"1" orelse hd stack = #"2" orelse hd stack = #"3" orelse hd stack = #"4" orelse hd stack = #"5" orelse hd stack = #"6") then 
                                            if i<= size(s) -2 andalso substring(s,i,1) = "*" andalso substring(s,i,2)="**" then 
                                            (i+2,#"b"::stack,text^"<b>")
                                            else if substring(s,i,1) = "*" then 
                                            (i+1,#"i"::stack,text^"<i>")
                                            else if substring(s,i,1) = "_" then 
                                            (i+1,#"u"::stack,text^"<u>")
                                            else if substring(s,i,1) = "\\"  then 
                                            (i+2,stack,text^substring(s,i+1,1))
                                            else if substring(s,i,1) = "\n" then 
                                            (i+1,tl stack,text^"</h"^(implode([hd stack]))^">")
                                            else(i+1,stack,text^substring(s,i,1))

                                            else if (hd stack = #"k") then 
                                            let val x = charfind(s,i,#"]")
                                                val y = substring(s,x+1,1)
                                                
                                            
                                             in if (y = "(") then let val z = charfind(s,x+1,#")") in (z+1,tl stack,text^"<a href=\""^substring(s,x+2,z-x-2)^"\">"^substring(s,i+1,x-i-1)^"</a>") end else(i+1, tl stack,text^"[") end
                                            else if (hd stack = #"l") then 
                                            let val x = charfind(s,i,#">") in (x+1,tl stack , text^"<a href=\""^substring(s,i+1,x-i-1)^"\">"^substring(s,i+1,x-i-1)^"</a>") end

                                            
                                            else if (hd stack = #"o") then (*outside any tag mode*) (*always append one newline character*)
                                            if substring(s,i,1) = "\n" then 
                                            (i+1,stack,text)
                                            else if 1<=headfind(s,0,i)  then
                                            let val x = chr(headfind(s,0,i)+48) in 
                                            (i+headfind(s,0,i),x::stack,text^"<h"^implode([x])^">") end
                                            else if substring(s,i,3) = "<<\n" then 
                                            (i+3,#"t"::stack,text^"<center><table border =\"1\"><tr><td>")
                                            else if substring(s,i,3) = "---" then 
                                            let val x = charfind(s,i,#"\n") in (x+1,stack,text^"<hr>")end 

                                            else (i,#"n"::stack,text^"<p>")

                                            else if (hd stack = #"t") then 
                                            if substring(s,i,3) = "\n>>" then (i+3,tl stack, text^"</td></tr></table></center>")
                                            else if substring(s,i,1) = "\n" then (i+1,stack,text^"</td></tr><tr><td>")
                                            else if substring(s,i,1) = "\\"  then (i+2,stack,text^substring(s,i+1,1))
                                            else if substring(s,i,1) = "|" then (i+1,stack,text^"</td><td>")
                                            else if i<= size(s) -2 andalso substring(s,i,1) = "*" andalso substring(s,i,2)="**" then 
                                            (i+2,#"b"::stack,text^"<b>")
                                            else if substring(s,i,1) = "*" then 
                                            (i+1,#"i"::stack,text^"<i>")
                                            else if substring(s,i,1) = "_" then 
                                            (i+1,#"u"::stack,text^"<u>")
                                            else if substring(s,i,5) = "<http" then 
                                            (i,#"l"::stack,text)
                                            else if substring(s,i,1) = "[" then 
                                            (i,#"k"::stack, text)
                                            else (i+1,stack,text^substring(s,i,1))

                                            
                                            

                            
                            
                            
                                            else if(hd stack = #"n") then 
                                            
                                            if i<= size(s) -2 andalso substring(s,i,1) = "*" andalso substring(s,i,2)="**" then 
                                            (i+2,#"b"::stack,text^"<b>")
                                            else if substring(s,i,1) = "*" then 
                                            (i+1,#"i"::stack,text^"<i>")
                                            else if substring(s,i,1) = "_" then 
                                            (i+1,#"u"::stack,text^"<u>")
                                            else if substring(s,i,1) = "\\"  then 
                                            (i+2,stack,text^substring(s,i+1,1))
                                            else if substring(s,i,5) = "<http" then 
                                            (i,#"l"::stack,text)
                                            else if substring(s,i,1) = "[" then 
                                            (i,#"k"::stack, text)
                                            else if substring(s,i,1) = "\n" then 
                                            if i = size(s)-1 then (i+1,tl stack, text^"</p>") else if (substring(s,i+1,1) = "\n" orelse substring(s,i+1,2) = "<<" orelse substring(s,i+1,3)="---") then (i+1,tl stack, text^"</p>")  else (i+1,stack,text^substring(s,i,1))
                                            else if 1<=headfind(s,0,i) then 
                                            (i,tl stack,text^"</p>")
                                            
                                            else (i+1,stack,text^substring(s,i,1))

                                        
                            
                                        else  if(hd stack= #"u") then 
                                        if substring(s,i,1) = "*" andalso substring(s,i,2)="**" then 
                                            (i+2,#"b"::stack,text^"<b>")
                                            else if substring(s,i,1) = "*" then 
                                            (i+1,#"i"::stack,text^"<i>")
                                            else if substring(s,i,1) = "_" then 
                                            
                                            (i+1,tl stack,text^"</u>")
                                            else if substring(s,i,1) = "\\"  then 
                                            (i+2,stack,text^substring(s,i+1,1))
                                            
                                            else (i+1,stack,text^substring(s,i,1))
                                        else  if(hd stack = #"b") then 
                                        if substring(s,i,1) = "*" andalso substring(s,i,2)="**" then 
                                            (i+2,tl stack,text^"</b>")
                                            else if substring(s,i,1) = "*" then 
                                            (i+1,#"i"::stack,text^"<i>")
                                            else if substring(s,i,1) = "_" then 
                                            
                                            (i+1,#"u":: stack,text^"<u>")
                                            else if substring(s,i,1) = "\\"  then 
                                            (i+2,stack,text^substring(s,i+1,1))
                                            
                                            else (i+1,stack,text^substring(s,i,1))
                                        else  if(hd stack = #"i") then 
                                        if substring(s,i,1) = "*" andalso substring(s,i,2)="**" then 
                                            (i+2,#"b":: stack,text^"<b>")
                                            else if substring(s,i,1) = "*" then 
                                            (i+1,tl stack,text^"</i>")
                                            else if substring(s,i,1) = "_" then 
                                            
                                            (i+1,#"u":: stack,text^"<u>")
                                            else if substring(s,i,1) = "\\"  then 
                                            (i+2,stack,text^substring(s,i+1,1))
                                            
                                            else (i+1,stack,text^substring(s,i,1))
                                        else raise Stupid


                                        (*if stack is empty at the end that means some operator was not closed*)


                        
                         in lineparse2(nextpos,stackn,ntext) end 




                in lineparse2(0,[#"o"],"") end



(*fun textparser(text: string) : string*string = let

                                                    fun matchi(i: int) (* i denotes the string 0 to i that im checking for matching*)  = 
                                                    if i = size(text) then raise parseError

                                                    else if matcher(substring(text,0,i),hrregex) then 
                                                    ("<hr>",substring(text,i+1,size(text)-i-1))
                                                    else if 1<=headfind(text,0) andalso headfind(text,0)<=6  andalso substring(text,i,1)="\n" then (*use let in end here later*)
                                                     ("<h"^Int.toString(headfind(text,0))^">"^lineparse1(substring(text,headfind(text,0),i-headfind(text,0)+1))^"</h"^Int.toString(headfind(text,0))^">",substring(text,i+1,size(text)-i-1))

                                                    
                                                    

                                                    else matchi(i+1)
                                                    
                                                    
                                                    
                                                    in matchi(0) end
                                                    



      *)              

    
fun mdt2html(filename: string) = 
    let
val file = readFile(filename)^"\n"

val parsed = lineparse1(file)
val z = writeFile(substring(filename,0,size(filename)-3)^"html",parsed)

    in 
z
    end


