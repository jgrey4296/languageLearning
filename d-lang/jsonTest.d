import std.stdio;
import std.json;


void main(){

//parse a file or string of json into a usable structure
string s = "{ \"language\": \"D\", \"rating\": 3.14, \"code\": \"42\" }";
JSONValue j = parseJSON(s);
writeln("Language: ", j["language"].str(),
        " Rating: ", j["rating"].floating()
       );

// j and j["language"] return JSONValue,
// j["language"].str returns a string

//check a type
long x;
if (j["code"].type() == JSON_TYPE.INTEGER)
{
    x = j["code"].integer;
}
else
{
    x = to!int(j["code"].str);
}

// create a json struct
JSONValue jj = [ "language": "D" ];
// rating doesnt exist yet, so use .object to assign
jj.object["rating"] = JSONValue(3.14);
// create an array to assign to list
jj.object["list"] = JSONValue( ["a", "b", "c"] );
// list already exists, so .object optional
jj["list"].array ~= JSONValue("D");

s = j.toString();
writeln(s);

}
