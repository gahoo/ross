var aria2 = new Aria2();
var download_dir;
var setDownloadDir = function(){
  aria2.getGlobalOption(function(err, res){
  download_dir = res.dir;
  });
};

var updateCWD = function(key){
  cwd = document.getElementById("cwd").value;

  if(key == '..'){
    cwd_array = cwd.split('/');
    console.log(naviTo);
    naviTo = cwd_array.slice(0, cwd_array.length - 2).join('/');
    if(naviTo !== ''){
      naviTo = naviTo + '/';
    }
  }else{
    naviTo = cwd + key;
  }

  document.getElementById("cwd").value = naviTo;
  Shiny.onInputChange("cwd", naviTo);
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
