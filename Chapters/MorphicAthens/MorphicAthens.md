## Integration of Athens in Spec

This chapter has been originally written by renaud villemeur. We thank him for this contribution. It shows how you can integrate vector graphic drawing within Spec component.

### Introduction

There are two different computer graphics: vector and raster graphics. 
Raster graphics represents images as a collection of pixels. Vector graphics 
is the use of geometric primitives such as points, lines, curves, or polygons 
to represent images. These primitives are created using mathematical equations.

Both types of computer graphics have advantages and disadvantages. 
The advantages of vector graphics over raster are:
- smaller size
- ability to zoom indefinitely
- moving, scaling, filling, and rotating does not degrade the quality of an image

Ultimately, picture on a computer are displayed on a screen with a specific 
display dimension. However, while raster graphic doesn't scale very well when
the resolution differ too much from the picture resolution, vector graphics
are rasterized to fit the display they will appear on. Rasterization is the 
technique of taking an image described in a vector graphics format and 
transform it into a set of pixels for output on a screen.

##### Note. 
You have the same concept when doing 3D programming with an API like openGL. You describe your scene with point, vertices, etc..., and in the end, you rasterize your scene to display it on your screen.

Morphic is currently the way to go on Pharo for Graphics. 
However, most existing canvas are pixel based, and not vector based. 
This can be an issue with current IT ecosystems, where the resolution can differ from machine to machine (desktop, tablet, phones, etc...)

Enter Athens, a vector based graphic API. Under the scene, it can either use
balloon Canvas, or the cairo graphic library for the rasterization phase.

When you integrate Athens with Spec, you'll use its rendering engine to 
create your picture. 
It's then transformed in a `Form` and displayed on the screen.

### Hello-world in Athens

We'll see how to use Athens directly integrated with Morphic. 
This is why we first start to create a Morph class 
It will be the class we'll use after for all our experiment:

First, we define a class, which inherit from `Morph`:
```language=Smalltalk
Morph << #AthensHello
	slots: { #surface }; 	
	package: 'Athens-Hello'
```


During the initialization phase, we'll create our Athens surface:
```language=Smalltalk
AthensHello >> initialize
	super initialize.
	self extent: self defaultExtent.
	surface := AthensCairoSurface extent: self extent.
```

where `defaultExtent` is simply defined as
```language=Smalltalk
AthensHello >> defaultExtent
???
```


The `drawOn:` method, mandatory in Morph subclasses, asks Athens to render
its drawing, and it'll then display it in a Morphic canvas as a Form (a bitmap 
pictures)

```language=Smalltalk
AthensHello >> drawOn: aCanvas
	self renderAthens.
	surface displayOnMorphicCanvas: aCanvas at: bounds origin.
```


Our actual Athens code is located into `renderAthens` method:, and the result is
stored in the surface instance variable.

```language=Smalltalk
AthensHello >> renderAthens
	| font |
	font := LogicalFont familyName: 'Arial' pointSize: 10.
	surface drawDuring: [:canvas | 
		"canvas pathTransform loadIdentity."
		surface clear. 
		canvas setPaint: ((LinearGradientPaint from: 0@0  to: self extent) colorRamp: {  0 -> Color white. 1 -> Color black }).
		canvas drawShape: (0@0 extent: self extent). 
		canvas setFont: font. 
		canvas setPaint: Color pink.
		canvas pathTransform translateX: 20 Y: 20 + (font getPreciseAscent); scaleBy: 2; rotateByDegrees: 25.
		canvas drawString: 'Hello Athens in Pharo/Morphic'
		
	].
```


To test your code, let's add an helper method. This will add a button on the left
of the method name. When you click on it, it'll execute the content of the 
script instruction.

```language=Smalltalk
AthensHello >> open
	<script: 'self new openInWindow'>
```


### One last thing: Handling resizing

You can already create the window, and see a nice gradient, with 
a greeting text. However, you'll notice, if you resize your window, that the 
Athens content is not resized. To fix this, we'll need one last method.

```language=Smalltalk
AthensHello >> extent: aPoint
	| newExtent |
	newExtent := aPoint rounded.
	(bounds extent closeTo: newExtent)
		ifTrue: [ ^ self ].
	self changed.
	bounds := bounds topLeft extent: newExtent.
	surface := AthensCairoSurface extent: newExtent.
	self layoutChanged.
	self changed
```


Congratulation, you have now created your first morphic windows where content
is rendered using Athens. 


### Using your morph with Spec

To integrate your morph:
```language=Smalltalk
SpPresenter >> newMorph
	^ self instantiate: SpMorphPresenter
```


Usage in your own presenter: 
```language=Smalltalk
OwnPresenter >> initializePresenters
	demo := self newMorph.
	demo morph: AthensDemoMorph new.
```


### Direct integration of Athens with Spec

In your presenter:
```language=Smalltalk
MyPresenter >> initializePresenters
    paint := self instantiate: SpAthensStaticPresenter.
	paint surfaceExtent: 600@400.
	paint drawBlock: [ :canvas | self render: canvas ].
```


The `render:` method:

```language=Smalltalk
MyPresenter >> render: canvas
    canvas
        setPaint:
            (canvas surface
                createLinearGradient:
                    {(0 -> Color white).
                    (1 -> Color black)}
                start: 0@0
                stop: canvas surface extent).
    canvas drawShape: (0 @ 0 extent: canvas surface extent).
```


The window definition
```language=Smalltalk
MyPresenter >> initializeWindow: aWindowPresenter
	aWindowPresenter
		title: 'Draw athens on Spec';
		initialExtent: 600@400;
		windowIcon: self windowIcon;
		askOkToClose: false;
		aboutText: 'Draw athens on Spec'
```



%todo 
%- find how I can resize the surface with I resize the window
%- find how I can integrate keyboard and mouse action with the presenter.


###  Athens details

`AthensSurface` and its subclass `AthensCairoSurface` will initialize a new surface.
The surface represent the area in pixel where your drawing will be rendered. You 
never draw directly on the surface. Instead, you specify what you want to display
 on the canvas, and Athens will render your it on the area specified by the surface. 

**AthensCanvas** is the central object used to perform drawing on an **AthensSurface**
Canvas is not directly instanciated but used through a call like 
"surface drawDuring: \[:canvas | .... \]"

The Athens drawing model relies on a three layer model. Any drawing process 
takes place in three steps:

- First a `path` is created, which includes one or more vector primitives , i.e., circles, lines, TrueType fonts, Bézier curves, etc...
- Then painting must be defined, which may be a color, a color gradient, a bitmap or some vector graphics
- Finally the result is drawn to the Athens surface, which is provided by the back-end for the output.


### Path

Athens always has an active path. 

Use `AthensPathBuilder` or `AthensSimplePathBuilder` to build a path
They will assemble segment for you

The method `createPath:` exists in all important Athens class:
- AthensCanvas
- AthensSurface
- AthensPathBuilder

The message `createPath: aPath`

Using it return a new path:

[[[language=smalltalk
surface createPath: [:builder |
		builder
			absolute;
			moveTo: 100@100;
			lineTo: 100@300;
			lineTo: 300@300;
			lineTo: 300@100;
			close ].
]]]

Here are some helper messages in `AthensSimplePathBuilder`:
- `pathStart`
- `pathBounds`  gives the limit of the bounds associated to the path

If you want to build path using only straight line, you can use the class
`AthensPolygon`


|path builder Messages  |Object Segment     |comment                     |
|~~~~~~~~~~~-|~~~~~~~~~-|~~~~~~~~~~~~~~|
|ccwArcTo: angle:       |AthensCCWArcSegment|counter clock wise segment  |
|cwArcTo:angle:         |AthensCWArcSegment |clock wise segment          |
|lineTo:                |AthensLineSegment  |straight line               |
|moveTo:                |AthensMoveSegment  |start a new contour         |
|curveVia: to:          |AthensQuadSegment  |quadric bezier curve        |
|curveVia: and: to:     |AthensCubicSegment |Cubic bezier curve          |
|reflectedCurveVia: to: |AthensCubicSegment |Reflected cubic bezier curve|
|string: font:          |                   |specific to cairo           |
|close                  |AthensCloseSegment |close the current contour   |


### Coordinate class: **Absolute** or **Relative**

absolute: absolute coordinate from surface coordinate.
This will draw a square in a surface which extent is 400@400 using absolute move.

[[[language=smalltalk

builder absolute;
			moveTo: 100@100;
			lineTo: 100@300;
			lineTo: 300@300;
			lineTo: 300@100;
			close
]]]


relative: each new move is relative to the previous one.
This will draw a square in a surface which extent is 400@400 using relative move.
[[[language=smalltalk
	builder relative ;
		moveTo: 100@100;
		lineTo: 200@0;
		lineTo: 0@200;
		lineTo: -200@0;
		close
]]]

cwArcTo:angle: and ccwArcTo: angle: will draw circular arc, connecting  
previous segment endpoint and current endpoint of given angle, passing in 
clockwise or counter clockwise direction. The angle must be specified in Radian.

% Please remember that the circumference of a circle is equal to 2 Pi  R.
% If R = 1, half of the circumference is equal to PI, which is the value of half
% a circle.

#### curveVia: to: and |curveVia: and: to:
This call is related to bezier curve. A Bézier curve consists of two or more
 control points, which define the size and shape of the line. The first and 
 last points mark the beginning and end of the path, while the intermediate 
 points define the path's curvature.
 
 More detail on Bezier curve on available at: https://pomax.github.io/bezierinfo/

####  path transformation.
A path can be rotated, translated and scaled so you can adapt it to your need.
For example, you can define a path in your own coordinate system, and then 
scale it to match the size of your surface extent.


### The different type of painting.

Paints can be created either from the surface or directly from a class that will
do the call to the surface for you.

any object can be treated as paint:
 - `athensFillPath: aPath on: aCanvas`
 - `athensFillRectangle: aRectangle on: aCanvas`
 - `asStrokePaint`

|surface message                                  |  comment               |
|~~~~~~~~~~~~~~~~~~~~~~~~-|~~~~~~~~~~~~|
|createFormPaint:                                 |create paint from a Form|
|createLinearGradient: start: stop:               |linear gradient paint   |
|createRadialGradient: center: radius:            |Radial gradient paint   |
|createRadialGradient: center: radius: focalPoint:|Radial gradient paint   |
|createShadowPaint:                               |???                     |
|createSolidColorPaint:                           |fill paint              |
|createStrokePaintFor:                            |stroke paint            |

a Canvas define its paint method we will see in detail below:
 - setPaint:
 - setStrokePaint:

# Stroke paint (a pen that goes around the path)
The **createStrokePaintFor** operation takes a virtual pen along the path. It allows the source 
to transfer through the mask in a thin \(or thick\) line around the path

AthensStrokePaint return a stroke paint.



!!! Solid paint \(a pen that fill the path\)
The **createSolidColorPaint** operation instead uses the path like the lines of a coloring book, 
and allows the source through the mask within the hole whose boundaries are the 
path. For complex paths \(paths with multiple closed sub-paths—like a donut—or
paths that self-intersect\) this is influenced by the fill rule


!!! Gradient
Gradient will let you create gradient of color, either linear, or radial.

The color ramp is a collection of associations with keys - floating point values 
between 0 and 1 and values with Colors, for example:
{0 -> Color blue . 0.5 -> Color white. 1 -> Color red}.

You can use either helper class or calling surface messages:
```language=smalltalk
surface createLinearGradient: {0 -> Color blue .0.5 -> Color white. 1 -> Color red} start:  0@0  stop: 200@100.
```
or
```language=smalltalk
```


Start and stop point are reference to the current shape being drawn.
Exemple:
Create a vertical gradient
```language=smalltalk
canvas
    setPaint:
        \(canvas surface
            createLinearGradient:
                {\(0 -> Color blue\).
                \(0.5 -> Color white\).
                \(1 -> Color red\)}
            start: 0@200
            stop: 0@400\). 
    canvas drawShape: \(0@200 extent: 300@400\).
```

create a horizontal gradient:
```language=smalltalk
canvas
    setPaint:
        \(canvas surface
            createLinearGradient:
                {\(0 -> Color blue\).
                \(0.5 -> Color white\).
                \(1 -> Color red\)}
            start: 0@200
            stop: 300@200\). 
    canvas drawShape: \(0@200 extent: 300@400\).
```

create a diagonal gradient:
```language=smalltalk
canvas
    setPaint:
        \(canvas surface
            createLinearGradient:
                {\(0 -> Color blue\).
                \(0.5 -> Color white\).
                \(1 -> Color red\)}
            start: 0@200
            stop: 300@400\). 
    canvas drawShape: \(0@200 extent: 300@400\).
```


### drawing

Either you set the shape first and then you call **draw**, or you call the 
convenient method **drawShape:** directly with the path to draw as argument

### Some example:

```language=smalltalk
"canvas pathTransform loadIdentity.  font1 getPreciseAscent. font getPreciseHeight"
			surface clear.
			canvas
				setPaint:
					((LinearGradientPaint from: 0 @ 0 to: self extent)
						colorRamp:
							{(0 -> Color white).
							(1 -> Color black)}).
			canvas drawShape: (0 @ 0 extent: self extent).
			canvas
				setPaint:
					(canvas surface
						createLinearGradient:
							{(0 -> Color blue).
							(0.5 -> Color white).
							(1 -> Color red)}
						start: 0@200
						stop: 0@400). "change to 200 to get an horizontal gradient"
			canvas drawShape: (0@200 extent: 300@400).
			canvas setFont: font.
			canvas
				setPaint:
					(canvas surface
						createLinearGradient:
							{(0 -> Color blue).
							(0.5 -> Color white).
							(1 -> Color red)}
						start: 50@0
						stop: (37*5)@0). "number of caracter * 5"
			canvas pathTransform
				translateX: 45 Y: 45 + font getPreciseAscent;
				scaleBy: 2;
				rotateByDegrees: 28.
			canvas
				drawString: 'Hello Athens in Pharo/Morphic !!!!!!!'.
```

                
```language=smalltalk
renderAthens
	surface
		drawDuring: [ :canvas | 
			| stroke squarePath circlePath |
			squarePath := canvas
				createPath: [ :builder | 
					builder
						absolute;
						moveTo: 100 @ 100;
						lineTo: 100 @ 300;
						lineTo: 300 @ 300;
						lineTo: 300 @ 100;
						close ].
			circlePath := canvas
				createPath: [ :builder | 
					builder
						absolute;
						moveTo: 200 @ 100;
						cwArcTo: 200 @ 300 angle: 180 degreesToRadians;
						cwArcTo: 200 @ 100 angle: Float pi ].
			canvas setPaint: Color red.
			canvas drawShape: squarePath.
			stroke := canvas setStrokePaint: Color black.
			stroke
				width: 10;
				joinRound;
				capRound.
			canvas drawShape: squarePath.
			canvas drawShape: circlePath.
			circlePath := canvas
				createPath: [ :builder | 
					builder
						relative;
						moveTo: 175 @ 175;
						cwArcTo: 50 @ 50 angle: 180 degreesToRadians;
						cwArcTo: -50 @ -50 angle: Float pi ].
			canvas drawShape: circlePath ]
```

                
### practicing Athens drawing.

To help you practice your Athens drawing, you can use Athens sketch, migrated from SmalltalkHub that is available at 
[Athens Sketch: https://github.com/rvillemeur/AthensSketch](https://github.com/rvillemeur/AthensSketch)
