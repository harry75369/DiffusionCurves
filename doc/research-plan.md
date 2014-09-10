# Research Plan Documentation

Vector graphics is important. With the development of screen of higher and 
higher resolution, the shortcomings of raster graphics are becoming more and 
more obvious, e.g. limited resolution, big file size and the most important, 
the mosaic effect. Although mosaic effect can be seen as a kind of art 
sometimes, it is not wanted in most use cases. In contrast, vector graphics has 
unlimited resolution and smaller file size, and it also can eliminate mosaic 
effect at arbitrary resolution.

One shortcoming of vector graphics, however, is that it takes more overheads 
to get rendered compared with raster graphics[?]. This problem is further 
worsen by the appearance of diffusion curve, which is a new primitive in
vector graphics for arbitrary gradual color change effect but requires more 
computing power. Fortunately, with modern hardware and advanced algorithms, 
this problem is no longer a crucial one. The crucial problems for now are as 
follows:

 * How can vector graphics achieve the same quality of raster graphics? Here 
   the quality means the expressive richness provided by raster graphics.

 * How can vector graphics transcend the expressive level of raster graphics, 
   maybe through non-photorealistic rendering techniques and techniques that well
   utilizes the human vision system?

 * Is there any possibility for new vector graphics primitives, for example, 
   "advection" curve inspired by "diffusion" curve?

Another shortcoming of vector graphics is the modeling process. Traditional 
creation process of vector graphics is counter intuitive and time-consuming.  
This can be improved by introducing new modeling method, i.e. Vector Graphics 
Complex.

In a word, there are still space for improvements of the modeling and the 
rendering of vector graphics. The expressiveness of vector graphics is our main
research focus.
