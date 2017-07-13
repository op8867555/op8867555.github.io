var drawer = document.querySelector('#drawer');
var headers = document.querySelectorAll('#post-body > h1, #post-body > h2');

if (headers.length > 0) {
  drawer.appendChild(document.createElement('hr'));
}

headers.forEach(function(x) {
  var elem = document.createElement('a');
  elem.classList.add("mdl-navigation__link");
  elem.href = '#' + x.id;
  elem.innerHTML = x.innerHTML.toUpperCase();
  if (+x.tagName[1] == 1)
    elem.innerHTML = '<b>' + elem.innerHTML + '</b>';
  else
    elem.innerHTML =  '　' + elem.innerHTML;
  drawer.appendChild(elem);
})
