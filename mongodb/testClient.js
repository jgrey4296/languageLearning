var MongoClient = require('mongodb').MongoClient,
    assert = require('assert'),
    ObjectId = require('mongodb').ObjectID,
    url = 'mongodb://localhost:27017/myTest';

var testDoc = {
        "address" : {
            "street" : "2 Avenue",
            "zipcode" : "10075",
            "building" : "1480",
            "coord" : [ -73.9557413, 40.7720266 ]
        },
        "borough" : "Manhattan",
        "cuisine" : "Italian",
        "grades" : [
            {
                "date" : new Date("2014-10-01T00:00:00Z"),
                "grade" : "A",
                "score" : 11
            },
            {
                "date" : new Date("2014-01-16T00:00:00Z"),
                "grade" : "B",
                "score" : 17
            }
        ],
        "name" : "Vella",
        "restaurant_id" : "41704620"
};

MongoClient.connect(url, function(err,db){
    assert.equal(null,err);
    console.log("Connected correctly");
    //insertDoc(db,'restaurants',testDoc)
    var promises = [
        insertDoc(db,"restaurants",{
            _id : new ObjectId(),
            name : "blah"
        }),
        findRestaurants(db),
        findIndividual(db,{
            _id : new ObjectId("56c673d82a7d707e85988bd0")
        }),
        findAll(db,{
            //borough : "Queens"
        })
    ];

    Promise.all(promises)
        .catch(function(err){
            console.log("Error: ", err);
        })
            .then(function(val){
                console.log("Closing");
                db.close();
            });
        
    
});

var findRestaurants = function(db){
    var p = new Promise(function(resolve,reject){
        resolve(db.collection('restaurants').find());
    });
        p.then(function(cursor){
            return cursor.toArray();
        })
        .then(function(docs){
            console.log("Number of documents: ", docs.length);
            docs.slice(0,20).forEach(function(d){
                console.log(`Name: ${d.name}  ----  ${d._id}`);
            });
            return docs;
        })
    return p;
};

var findIndividual = function(db,criteria){
    var p = db.collection('restaurants').findOne(criteria)
        .then(function(val){
            //console.log("find ind: ", val);
            if(val === null) throw new Error("Val is null");
        console.log("Ind result: " + val.name);
        return val;
        })
        .catch(function(e){
            console.log("Error: " + e.message);
        });
    return p;
};

var findAll = function(db,criteria){
    console.log("Finding all");
    return db.collection('restaurants').find(criteria).limit(20).toArray()
        .then(function(arr){
            if(arr.length === 0) throw new Error("nothing found");
            arr.forEach(function(d){
                console.log(`Findall result: ${d.name} --- ${d.borough}`);
            });
            return arr;
        })
        .then(function(arr){
            console.log(arr);
        })
        .catch(function(e){
            console.log("Findall error: " + e.message);
        });
};


var insertDoc = function(db,collection,document){
    return new Promise(function(resolve,reject){
        db.collection(collection).insertOne(document,
        function(err,result){
            if(err !== null) { reject("Insertion Error: " + err); }
            resolve({
                dataBase : db,
                result : result
            });
        });
    });
};


