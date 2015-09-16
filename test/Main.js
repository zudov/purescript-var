// module Test.Main

exports.getCounter = function() {
  return global.counter || 0;
}

exports.setCounter = function(x) {
  return function() {
    global.counter = x;
    return {};
  }
}
