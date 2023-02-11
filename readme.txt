Design Decisions :-
 _ is used as a keyword, and needs to be closed like parantheses. Eg. _a bcd f_--> <u>a bcd f</u>
For parsing a paragraph, I will be making a pushdown automaton that maintains the stack which contains the information of which is the most recent tag that is being used
 Using \ inside a paragraph will escape special characters *,_,** . So writing this \* will render to normal asterik.
 While making a table , a new line has to come after placing <<. This restriction doesnot hamper the user's ability to make tables. Also while ending >> has to come after a newline
 There shouldn't be unnecessary newlines inside the table
    so <<1|2|3 is not allowed 
    neither is this <<
                        1|2|3>>
 If you write ---, all text after it would be ignored and a hr will be placed. If you want to write --- in a paragraph as a text then you must use the escape character
 If there are unmatched _,*,** this is an error and I have raised an unmatched exception in this case
 if () are ot used after [] in links then that won't be treated as a link. []() they need to be together for link to work. this doesn't hinder any functionality