var coll = document.getElementsByClassName("code_fold_it");
var i;
for (i = 0; i < coll.length; i++) {
  coll[i].addEventListener("click", function() {
    this.classList.toggle("active");
    var content = this;
    if (content.style.height === "1em") {
      content.style.overflowX = "auto";
      content.style.height = "auto";
      content.innerHTML = content.innerHTML.replace('<p class="placeholder_txt">Code <i class="arrow down"></i></p><br>',"");
    } else {
      content.style.overflow = "hidden";
      content.style.height = "1em";
      content.innerHTML = '<p class="placeholder_txt">Code <i class="arrow down"></i></p><br>' + content.innerHTML;
    }
  });
}



var coll = document.getElementsByClassName("code_fold_it_collapsed");
var i;
for (i = 0; i < coll.length; i++) {
  coll[i].style.overflow = "hidden";
  coll[i].style.height = "1em";
  coll[i].innerHTML = '<p class="placeholder_txt">Code <i class="arrow down"></i></p><br>' + coll[i].innerHTML;
  coll[i].addEventListener("click", function() {
    this.classList.toggle("active");
    var content = this;
    if (content.style.height === "1em") {
      content.style.overflowX = "auto";
      content.style.height = "auto";
      content.innerHTML = content.innerHTML.replace('<p class="placeholder_txt">Code <i class="arrow down"></i></p><br>',"");
    } else {
      content.style.overflow = "hidden";
      content.style.height = "1em";
      content.innerHTML = '<p class="placeholder_txt">Code <i class="arrow down"></i></p><br>' + content.innerHTML;
    }
  });
}






var coll = document.getElementsByClassName("example_data_fold_it");
var i;
for (i = 0; i < coll.length; i++) {
  coll[i].addEventListener("click", function() {
    this.classList.toggle("active");
    var content = this;
    if (content.style.height === "1em") {
      content.style.overflowX = "auto";
      content.style.height = "auto";
      content.innerHTML = content.innerHTML.replace('<p class="placeholder_txt">Bsp Daten <i class="arrow down"></i></p><br>',"");
    } else {
      content.style.overflow = "hidden";
      content.style.height = "1em";
      content.innerHTML = '<p class="placeholder_txt">Bsp Daten <i class="arrow down"></i></p><br>' + content.innerHTML;
    }
  });
}



var coll = document.getElementsByClassName("example_data_fold_it_collapsed");
var i;
for (i = 0; i < coll.length; i++) {
  coll[i].style.overflow = "hidden";
  coll[i].style.height = "1em";
  coll[i].innerHTML = '<p class="placeholder_txt">Bsp Daten <i class="arrow down"></i></p><br>' + coll[i].innerHTML;
  coll[i].addEventListener("click", function() {
    this.classList.toggle("active");
    var content = this;
    if (content.style.height === "1em") {
      content.style.overflowX = "auto";
      content.style.height = "auto";
      content.innerHTML = content.innerHTML.replace('<p class="placeholder_txt">Bsp Daten <i class="arrow down"></i></p><br>',"");
    } else {
      content.style.overflow = "hidden";
      content.style.height = "1em";
      content.innerHTML = '<p class="placeholder_txt">Bsp Daten <i class="arrow down"></i></p><br>' + content.innerHTML;
    }
  });
}





var coll = document.getElementsByClassName("result_fold_it");
var i;
for (i = 0; i < coll.length; i++) {
  coll[i].addEventListener("click", function() {
    this.classList.toggle("active");
    var content = this;
    if (content.style.height === "1em") {
      content.style.overflowX = "auto";
      content.style.height = "auto";
      content.innerHTML = content.innerHTML.replace('<p class="placeholder_txt">Result <i class="arrow down"></i></p><br>',"");
    } else {
      content.style.overflow = "hidden";
      content.style.height = "1em";
      content.innerHTML = '<p class="placeholder_txt">Result <i class="arrow down"></i></p><br>' + content.innerHTML;
    }
  });
}



var coll = document.getElementsByClassName("result_fold_it_collapsed");
var i;
for (i = 0; i < coll.length; i++) {
  coll[i].style.overflow = "hidden";
  coll[i].style.height = "1em";
  coll[i].innerHTML = '<p class="placeholder_txt">Result <i class="arrow down"></i></p><br>' + coll[i].innerHTML;
  coll[i].addEventListener("click", function() {
    this.classList.toggle("active");
    var content = this;
    if (content.style.height === "1em") {
      content.style.overflowX = "auto";
      content.style.height = "auto";
      content.innerHTML = content.innerHTML.replace('<p class="placeholder_txt">Result <i class="arrow down"></i></p><br>',"");
    } else {
      content.style.overflow = "hidden";
      content.style.height = "1em";
      content.innerHTML = '<p class="placeholder_txt">Result <i class="arrow down"></i></p><br>' + content.innerHTML;
    }
  });
}
