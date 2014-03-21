var http = require('http');

var program = function() {
    var program = {
       name: null,
       mood: null
    }
}


function sleep (args, app, kont, useless, useless2, useless3) {
   var time = args[0] * 1000;
   consoleF("Seelping for " + args[0] + " seconds");
   var date = new Date();
   var curDate = null;
   do { curDate = new Date(); }
   while(curDate-date < time);

}

function consoleF(str) {
   if(debug)
      console.log(str);
}

function setMood(args, app, kont, useless, useless2, useless3) {
   
   consoleF("Setting mood to " + args[0]);
   app.mood = args[0];
   notifyParent(app);
}


function loop(args, app, kont, useless, useless2, useless3) {

   //IF the length of the argrs if not 2 THEN
   if (args.length !== 2) {
      //write error message to the console
      console.write("loop error, invalid args length");
  } else {
      //SET len to total loop length length
      var len = args[0];
      //SET exp to expression to execute
      var exp = args[1];
      //FOR each loop length execute the expression
      while(len > 0) {
         consoleF("loop: " + len)
         //CALL the expression with the args and app
         getFunction(exp.ty) (exp.args, app, kont, null, null, false);
         len--;
      }
   }
}

function goto(args, app, kont, explist, beginWhere, uselist) {

   consoleF("goto "+ args[0] + " port : " + args[1]);
   var jsonString = null;   
   if(uselist) {
		var newExps = modifyList(explist, beginWhere);
		var kontObj = {ty: "seq-k", args: [newExps, kont]};
		var obj = {name: app.name, mood: app.mood, kont: kontObj};
		jsonString = JSON.stringify(obj);
   } else {
		var obj = {name: app.name, mood: app.mood, kont: kont};
		jsonString = JSON.stringify(obj);
   }



var options = {
        host: args[0],
        port: args[1],
        path: '/run.html',
        method: 'POST'
        };

   sendPost(options, jsonString);
   process.exit(0);
}


function modifyList(list, begin) {
	var newlist = [], index;
	for(index = begin; index < list.length; index++) {
		newlist.push(list[index]);
	}
	return newlist;
}


function seq (args, app, kont, useless, useless2, useless3) {
   var len = args.length, index, callingFunc;
   
   for (index = 0; index < len; index++) {
      consoleF("seq @ " + index);
	 
	 callingFunc = getFunction(args[index].ty)
	 //check if the exp is a goto, if so, save stuff to make serialization easier
	 if(callingFunc === goto && (index+1 < len))
		callingFunc(args[index].args, app, kont, args, index+1, true);
	 else
		callingFunc(args[index].args, app, kont, null, null, false);
   }
}

function baseK(args, app) {
   process.exit(0);
}

function loopK (args, app) {
   if(args.length != 3) {
     console.log("error loop-k invalid args length");
   } else {
      var len = args[0];
      var exp = args[1];
      var kont = args[2];
      
      while(len > 0) {
         consoleF("loopK " + len);
         getFunction(exp.ty)(exp.args, app, kont, null, null, false);
         len--;
      }
      getFunction(kont.ty)(kont.args, app);
   }
}

function seqK (args, app) {
  if (args.length != 2) {
     console.log("error seqK invalid args length");
  } else {
     var exp = args[0];
     var kont = args[1];
	 var callingFunc = null
	 
     var expLen = exp.length;
     var index = 0;
     for(index = 0; index < expLen; index++) {
         consoleF("seqK @ " + index);
		 callingFunc = getFunction(exp[index].ty)
		 //check if the exp is a goto, if so, save stuff to make serialization easier
		 if(callingFunc === goto && (index+1 < expLen))
			callingFunc(exp[index].args, app, kont, exp, index+1, true);
		 else
			callingFunc(exp[index].args, app, kont, null, null, false);
     }
     getFunction(kont.ty) (kont.args, app);
  } 

}

process.on('message', function(message) {
   consoleF("message recieved : " + message);
   if(message != null) {
      if(message.call == "interp") {
         if(message.args == null)
            consoleF("invalid args to process in interp");
         else {
            interp(message.args);
         }

      }
   }
});

function notifyParent(app) {
   process.send( {message : "notifyParent", 
                  childid: process.pid, 
                  app: app });
}

function sendPost (options, data) {
   process.send( {message : "sendPost",
                  options : options,
                  data : data});
} 



function interp(json) {
   app = new program();
   app.name = json.name;
   app.mood = json.mood;
   var kont = json.kont;
   notifyParent(app);
   consoleF("knot: " + kont);
   getFunction(kont.ty)(kont.args, app);

}

function getFunction(ty) {
  if(typeof handlers[ty] === 'function') {
     return handlers[ty];
  } else {
     console.log("invalid function");
  }
}

var debug = true;

var handlers = {}
handlers["base-k"] = baseK;
handlers["loop-k"] = loopK;
handlers["seq-k"] = seqK;
handlers["sleep"] = sleep;
handlers["set-mood"] = setMood;
handlers["goto"] = goto;
handlers["seq"] = seq;   

exports.interp = interp;
 
