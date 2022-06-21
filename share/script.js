function toggle(elt) {
  var elts,j;
  if (!elt) return;
  elts = elt.querySelectorAll("section-toggle");
  for (j = 0; j < elts.length; j++)
    elts[j].classList.toggle("active");
}

var nodes,i;

nodes = document.getElementsByClassName("section-open");
for (i = 0; i < nodes.length; i++) {
  nodes[i].addEventListener("click", function() {
    toggle(this);
  });
}

nodes = document.getElementsByClassName("section-close");
for (i = 0; i < nodes.length; i++) {
  nodes[i].addEventListener("click", function() {
    toggle(this.parentElement);
  });
}
