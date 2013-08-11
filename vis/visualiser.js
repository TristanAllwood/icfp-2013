module = angular.module('vis', []);

function Visualiser($scope, $timeout) {

  $scope.ws_status = "unopened";
  var ws = new WebSocket("ws://localhost:8000/", 'base64');

  ws.onopen = function() {
    $scope.$apply(function() { $scope.ws_status = "opened"; });
  };

  ws.onclose = function() {
    $scope.$apply(function() { $scope.ws_status = "lost"; });
  };

  var data = "{}";
  var msg = "";

  ws.onmessage = function(message) {
    var result = atob(message.data);

    msg = msg + result;

    var newline = -1;
    while((newline = msg.indexOf('\n')) > -1) {
      data = msg.substring(0, newline);
      msg = msg.substring(newline + 1, msg.length);
    }
  }

  $scope.pairs = [];

  $scope.train = function() {
     ws.send(btoa(JSON.stringify({ size: $scope.size })));
     ws.send(btoa('\n'));
  };

  function updateFromData() {
    $scope.data = JSON.parse(data);

    if($scope.data.programs === undefined) {
      return;
    }

    $scope.by_pair = [];

    $scope.data.seeds.forEach(function(seeds, idx) {

      $scope.by_pair.push( { input: seeds[0],
                             output: seeds[1],
                             count: $scope.data.counts[idx],
                             current_program: $scope.data.programs[idx]
                           });

    });

  }

  function loop() {
    $timeout(function() {
      $scope.$apply(function() {

        updateFromData();

      });
      loop();
    }, 1000);
  }


  $scope.size = 3;

  loop();

}
