var aria2 = new Aria2();
var download_dir;

aria2.getGlobalOption(function(err, res){
  download_dir = res.dir;
});

Shiny.addCustomMessageHandler("addLinks",
  function(message) {
    aria2.getGlobalOption(function(err, res){
      download_dir = res.dir;
    });

    var cnts = message.url.length;
    for (var i=0; i<cnts; i++){
      dir = download_dir + message.dir[i];
      aria2.addUri([message.url[i]], {'dir': dir});
    }
  }
);
