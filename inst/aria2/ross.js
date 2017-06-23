var aria2 = new Aria2();
var download_dir;
var setDownloadDir = function(){
  aria2.getGlobalOption(function(err, res){
  download_dir = res.dir;
  });
};

setDownloadDir();

Shiny.addCustomMessageHandler("addLinks",
  function(message) {
    setDownloadDir();

    if(download_dir === undefined){
      alert('Is aira2 running?');
      return;
    }

    var cnts = message.url.length;
    for (var i=0; i<cnts; i++){
      dir = download_dir + message.dir[i];
      aria2.addUri([message.url[i]], {'dir': dir});
    }
  }
);
