# ErlangExercises
Erlang applications for academic purposes

## math_server
Server evaluates arithmetic expressions: addition, subtraction, multiplication, division. Input data sent as parameters in the URL query. The first parameter specifies the arithmetic operation, the next - the next action arguments. The input language of HTTP requests in EBNF notation:
<pre>
<code>
URL                   = IP addr. & port No ,  request ;
IP addr. & port No    = “127.0.0.1:8080?” ;
request               = calculation , “&” , integer , { “&” , integer } ;
calculation           = “plus” , “minus”, “mult”, “div” ;
integer               = “0” | [ "-" ] , digit ;
digit                 = “0” | digit without 0 ;
digit without 0       = “1” |  “2” |  “3” |  “4” |  “5” |  “6” |  “7” |  “8” |  “9” ;
</code>
</pre>

<p align="center"><img src="https://github.com/kazimierczak-robert/ErlangExercises/blob/master/SC/math_server.PNG"></p>

## dict_rest
REST application for creating and managing a Polish-English dictionary. The available API includes methods (word ": word" is a variable meaning a word in Polish language which is the password in the dictionary):

<table>
  <tr>
    <th>Request</th>
    <th>Request Method</th>
    <th>Additional argument</th>
    <th>Meaning</th>
  </tr>
  <tr>
    <td>/list</td>
    <td>GET</td>
    <td>NO</td>
    <td>Display with meanings saved words</td>
  </tr>
  <tr>
    <td>/get/:word</td>
    <td>GET</td>
    <td>NO</td>
    <td>Display word: word with meaning</td>
  </tr>
  <tr>
    <td>/create/:word</td>
    <td>POST</td>
    <td>
      YES	
	    
  - header: Content-Type - application/x-www-form-urlencoded
  - body: content - word_meaning_:word
    </td>
    <td>Add word:word</td>
  </tr>
  <tr>
    <td>/update/:word</td>
    <td>POST</td>
    <td> 
      YES  
	    
  - header: Content-Type - application/x-www-form-urlencoded	
  - body: content - word_meaning_:word</td>
    <td>Update the meaning of the word: word</td>
  </tr>
  <tr>
    <td>/delete/:word</td>
    <td>DELETE</td>
    <td>NO</td>
    <td>Delete the word: word</td>
  </tr>
  <tr>
    <td>/help</td>
    <td rowspan="2">GET</td>
    <td rowspan="2">NO</td>
    <td rowspan="2">Display help (all available API commands)</td>
  </tr>
  <tr>
    <td>/</td>
  </tr>
</table>

Server methods:
<table>
  <tr>
    <th>Method</th>
    <th>Meaning</th>
  </tr>
  <tr>
    <td>allowed_methods/2</td>
    <td>Listing of supported HTTP requests</td>
  </tr>
  <tr>
    <td>content_types_provided/2</td>
    <td>Redirecting GET requests to the appropriate methods due to the format of the returned response</td>
  </tr>
  <tr>
    <td>content_types_accepted/2</td>
    <td>Redirecting POST requests to the appropriate methods due to the format of the query received</td>
  </tr>
  <tr>
    <td>db_to_json/2</td>
    <td>Redirecting GET queries with returned JSON response to the appropriate methods due to the type of API query</td>
  </tr>
  <tr>
    <td>db_to_text/2</td>
    <td>Redirecting GET queries with the returned response in text format to the appropriate methods due to the type of API query</td>
  </tr>
  <tr>
    <td>text_to_db/2</td>
    <td>Redirecting POST requests to the appropriate methods due to the type of API query</td>
  </tr>
  <tr>
    <td>delete_resource/2</td>
    <td>The method called when using the / delete method</td>
  </tr>
  <tr>
    <td>resource_exists/2</td>
    <td>A method that verifies word existing in the dictionary</td>
  </tr>
  <tr>
    <td>get_record_list/2</td>
    <td>A method that returns all saved words with meanings in JSON format</td>
  </tr>
  <tr>
    <td>get_record_list_text/2</td>
    <td>A method that returns all saved words with the meanings in a text format</td>
  </tr>
  <tr>
    <td>get_one_record/2</td>
    <td>The method returns a single word with the meaning in JSON format</td>
  </tr>
  <tr>
    <td>get_one_record_text/2</td>
    <td>The method that returns the word with the meaning in text format</td>
  </tr>
  <tr>
    <td>create_record_to_json/2</td>
    <td>The method that saves the new word with the meaning to the dictionary</td>
  </tr>
  <tr>
    <td>update_record_to_json/2</td>
    <td>The method updating the dictionary by updating existing word meaning</td>
  </tr>
  <tr>
    <td>get_help/2</td>
    <td>A method that returns all available API commands in JSON format</td>
  </tr>
  <tr>
    <td>get_help_text/2</td>
    <td>A method that returns all available API commands in text format</td>
  </tr>
</table>

Adding the word "mirror" with the meaning to the dictionary:

<p align="center"><img src="https://github.com/kazimierczak-robert/ErlangExercises/blob/master/SC/dict_rest.PNG"></p>

## ws_calc
Client-server appliacation using WebSocket protocol that reconstructs th operation of a simple calculator operating on a set of real numbers. The server provides mathematical operations:
- binary operations:
  - adding,
  - subtraction,
  - multiplication, 
  - division,
- unary operations:
  - square root,
  - reverse.

An equality operator completes calculations and displays the final result or reset calculations (AC). The client is an HTTP site with JavaScript scripts used to communicate with the server.

<p align="center"><img src="https://github.com/kazimierczak-robert/ErlangExercises/blob/master/SC/ws_calc.png"></p>
  
## Attributions
- http://davekuhlman.org/cowboy-rest-add-get-update-list.html
- https://freshman.tech/calculator/
 
## Credits
* Monika Grądzka
* Robert Kazimierczak
