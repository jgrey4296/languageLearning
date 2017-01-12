w = WebView(nil, (800 @ 200) @ (500 @ 400) )
w.html = "<body><p>heres a <a href=\"test.html\">link</a></p></body>";
w.onLinkActivated_({ |view,url| "test".postln })
w.onLinkActivated = nil
w.onReload = { "test".postln }

w.mouseDownAction = { "awefawe".postln }
w.onClose = {"awgjgji".postln }

w.front
w.close

