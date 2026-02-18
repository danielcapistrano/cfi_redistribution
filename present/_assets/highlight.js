document.addEventListener("DOMContentLoaded", function () {

  function animateMarksInSlide(slide) {
    const marks = slide.querySelectorAll("mark");
    marks.forEach(mark => {
      mark.classList.add("animate");
    });
  }

  // Animate marks on first slide
  animateMarksInSlide(Reveal.getCurrentSlide());

  // Animate marks when slide changes
  Reveal.on("slidechanged", function(event) {
    animateMarksInSlide(event.currentSlide);
  });

});

// if (!mark.classList.contains("animate")) {
//   mark.classList.add("animate");
// }