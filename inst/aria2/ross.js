var aria2 = new Aria2();
var download_dir;
var task_list = [];
var aria2_version;

var getVersion = function(){
  aria2.getVersion(function(err, res){
    aria2_version = res.version;
  });
  Shiny.onInputChange("aria2_version", aria2_version);
};

var setDownloadDir = function(){
  aria2.getGlobalOption(function(err, res){
    download_dir = res.dir;
  });
  // Shiny.onInputChange("download_dir", download_dir);
};

var setMaxCon = function(){
  var max_concurrent = $('#max_concurrent').val();
  aria2.changeGlobalOption({'max-concurrent-downloads': max_concurrent});
};

var setMaxOverallDonwloadLimit = function(){
  var max_overall_download_limit = $('#max_overall_download_limit').val();
  aria2.changeGlobalOption({'max-overall-download-limit': max_overall_download_limit});
};

var updateCWD = function(key){
  cwd = document.getElementById("cwd").value;

  if(key == '..'){
    cwd_array = cwd.split('/');
    naviTo = cwd_array.slice(0, cwd_array.length - 2).join('/');
    if(naviTo !== ''){
      naviTo = naviTo + '/';
    }
  }else{
    naviTo = cwd + key;
  }

  console.log(naviTo);
  document.getElementById("cwd").value = naviTo;
  Shiny.onInputChange("cwd", naviTo);
};

var setCWD = function(key){
  console.log(key);
  document.getElementById("cwd").value = key;
  Shiny.onInputChange("cwd", key);
};


var getTasks = function(err, res) {
  task_list = task_list.concat(res);
};

var updateTasks = function(){
  aria2.tellActive(getTasks);
  aria2.tellWaiting(0, 5000, getTasks);
    if(!$("#aria2_task_hide_stopped").is(':checked')){
      aria2.tellStopped(0, 5000, getTasks);
  }

  Shiny.onInputChange("tasks:aria2_tasks", task_list);
  console.log(task_list);
  task_list = [];
};

setDownloadDir();
getVersion();

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
