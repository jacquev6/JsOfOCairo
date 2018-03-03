Canvas = require('canvas');
Image = Canvas.Image;
fs = require('fs');
pixelmatch = require('pixelmatch');

writeTo = function(canvas, fileName) {
  var out = fs.createWriteStream(fileName);
  var stream = canvas.pngStream();
  stream.on('data', function(chunk) {
    out.write(chunk);
  });
}

require('./tests_in_node.bc.js')
