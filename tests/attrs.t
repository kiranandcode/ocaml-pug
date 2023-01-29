  $ ../bin/pug.exe ./resources/attrs.pug
  <a href="/contact">contact</a>
  <a href="/contact" class="body">contact</a>
  <a href="/contact" class="body">contact</a>
  <div><div></div></div>
  <div>div div , div div , random:<p>hello</p></div>
  <a href="static">static</a>
  <a href="/save" static="prueba" class="a button">save</a>
  <a bar="bar" baz="baz" foo="foo"></a>
  <a bar="1" foo="foo, bar, baz"></a>
  <a>(foo='((foo))', bar= (1 if 1 else 0 ))</a>
  <a class="col-#{2}"></a>
  <select><option selected="selected" value="foo">Foo</option>
   <option selected="selected" value="bar">Bar</option>
   <option selected="selected" value="missing">Qaz</option>
  </select>
