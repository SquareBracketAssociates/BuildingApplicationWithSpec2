## Styling Applications

@cha_style

In this chapter we will see how to use custom styles in Spec applications. We will start to present styles and then build a little editor as the one displayed hereafter.

We will show that an application in Spec manages styles and let you adapt the look of a presenter as shown in Figure *@style1@*.

![Building a little styling editor.](figures/style1.png width=80&label=style1)

We give some basis before showing how to effectively use styles to enhance the look and feel of an application.

### About stylesheet

Spec first collects the style for the presenter, then collects style for its
 sub-components. 'application' is the default root level, there is no
'application' adapter.

A defined stylesheet has to have always a root element, and this root element
needs to be called `'.application'`.

So each style follows a cascading style, starting from `.application` like
```
.application.label.header
.application.link
.application.checkBox
```

There are two ways to express stylesheets: one for Morphic expressed using an extended version of STON and CSS for Gtk.

### STON notation

Morphic styles can be declared using the STON notation.
Each style element can use specific properties defined by associated classes:

- Geometry: `SpStyleGeometry`
- Draw: `SpStyleDraw`
- Font:  `SpStyleFont`
- Container: `SpStyleContainer`

Example:
```
Geometry { #hResizing: true }
Draw { #color:  Color{ #red: 1, #green: 0, #blue: 0, #alpha: 1}}
Draw { #color: #blue}
Font { #name: "Lucida Grande", #size: 10, #bold: true }
Container { #borderColor: Color { #rgb: 0, #alpha: 0 }, #borderWidth: 2, #padding: 5 },
```

You can define your style globally, and to your specific presenter, with the `addStyle:`
message: for example `addStyle: 'section'`. 

This message is specific to the `SpAbstractMorphicAdapter` backend.
Here are two examples of stylesheets.

```
styleSheet
 ^ SpStyleSTONReader fromString: '
.application [
 Font { #name: "Source Sans Pro", #size: 10 },
 Geometry { #height: 25 },
 .label [
  Geometry { #hResizing: true },
  .headerError [Draw { #color:  Color{ #red: 1, #green: 0, #blue: 0, #alpha: 1}}  ],
  .headerSuccess [Draw { #color: Color{ #red: 0, #green: 1, #blue: 0, #alpha: 1}}  ],
  .header [
   Draw { #color: Color{ #rgb: 622413393 }},
   Font { #name: "Lucida Grande", #size: 10, #bold: true } ],
  .shortcut [
   Draw { #color: Color{ #rgb: 622413393 } },
   Font { #name: "Lucida Grande", #size: 10 } ],
  .fixed [
   Geometry { #hResizing: false, #width: 100 } ],
  .dim [
   Draw { #color : Color{ #rgb: 708480675 } } ]
 ],
 .link [  
  Geometry { #hResizing: true } ],
 .button [  
  Geometry { #width: 100 },
  .small [
     Geometry { #width: 26 } ] ],
 .checkBox [  
  Geometry { #hResizing: true } ],
 .radioButton [
  Geometry { #hResizing: true } ],
 .dropList [
  Geometry { #width: 150, #hResizing: true } ],
 .list [
  Geometry { #width: 150, #hResizing: true, #vResizing: true } ],
 .slider [
  Geometry { #width: 150, #hResizing: true } ],
 .actionBar [  
  Container {
   #borderColor: Color { #rgb: 0, #alpha: 0 },
   #borderWidth: 2,
   #padding: 5 },
  Geometry { #width: 150, #height: 29, #hResizing: true, #vResizing: false } ],
 .menuBar [
  Geometry { #width: 150, #hResizing: true } ],
 .actionButton [  
  Geometry { #width: 60, #hResizing: false },
  .showIcon [ Geometry { #width: 25 } ]  ],
 .toolBar [
  Geometry { #hResizing: true },
  .icons [
   Geometry { #height: 30 } ],
  .iconsAndLabel [  
   Geometry { #height: 45 } ] ],
 .text [
  Geometry { #height: 0 } ],
 .code [
  Font { #name : "Source Code Pro", #size : 10 } ],
 .codePopover [
  Draw { #color : #transparent },
  .button [
   Geometry { #width : 25 } ] ],
 .scrollbarPopoverLarge [
  Geometry { #height: 350 } ] ]
'
```

```
styleSheet
 ^ SpStyle defaultStyleSheet, (SpStyleSTONReader
  fromString:
   '
.application [
 Draw { #backgroundColor: #lightRed},
 .section [
   Draw { #color: #green, #backgroundColor: #lightYellow},
   Font {  #name: "Verdana", #size: 12, #italic: true, #bold: true}],
 .disabled [ Draw { #backgroundColor: #lightGreen} ],
 .textInputField [ Draw { #backgroundColor: #blue} ],
 .label [
  Font {  #name: "Verdana", #size: 10, #italic: false, #bold: true},
  Draw { #color: #red, #backgroundColor: #lightBlue} ] ]
')
```

### How do styles work?


Styles in Spec work like CSS. They are style sheets in which the properties for presenting a presenter are defined. Properties such as colors, width, height, font, and others.
As a general principle it is better to use styles instead of fixed constraints, because your application will be more responsive.

For example, you want a button to have a specific width and height.
You can do it using constraints with the method add:withConstraints: or using styles. In both cases the result will be as shown in Figure *@style2@*:

![Untitled window.](figures/style2.png width=50&label=style2)

But, if you change the size of the fonts of the Pharo image using Settings/Appearance/Standard Fonts/Huge, using fixed constraints, you will obtain the following result shown in Figure *@@*.
You will for example do not be able to see the icons because the size is not recomputed correctly.

![Badly scaled untitled window.](figures/style3.png width=50&label=style3)

While using styles,the size of the button will also scale as shown by Figure *@style4@*.

![Nicely scaled untitled window.](figures/style4.png width=50&label=style4)

### Anatomy of a style

The styles in Spec format are similar to CSS. 
Style style sheets are written using STON as format.
STON, the smalltalk textutal object notation is described in a dedicated chapter in the _Enterprise Pharo_ book available at [http://books.pharo.org](http://books.pharo.org).
We need to write the styles as a string and then parse it as a STON file.

Here is an example that we will explain steps by steps below.

```
'.application [
    .lightGreen [ Draw { #color: #B3E6B5 } ],
    .lightBlue [ Draw { #color: #lightBlue } ] ]'
```

	
We will go by steps.

`SpPropertyStyle` has 5 subclasses: `SpContainerStyle`, `SpDrawStyle`, `SpFontStyle`, `SpTextStyle`, and `SpGeometryStyle`. These subclasses define the 5 types of properties that exist. On the class side, the method `stonName` that indicates the name that we must put in the STON file.

!!todo stonName above is unclear

- `SpDrawStyle` modifies the properties related to the drawing of the presenter, such as the color and the background color.
- `SpFontStyle` manipulates all related to fonts.
- `SpGeometryStyle` is for sizes, like width, height, minimum height, etc.
- `SpContainerStyle` is for the alignment of the presenters, usually with property is changed on the main presenter, which is the one that contains and arranges the other ones.
- `SpTextStyle` controls the properties of the `SpTextInputFieldPresenter`.


If we want to change the color of a presenter, we need to create a string and use the `SpDrawStyle` property, which STON name is `Draw` as shown below. For setting the color, we can use either the hexadecimal code of the color or the sender of `Color` class.

```
'.application [
    .lightGreen [ Draw { #color: #B3E6B5 } ],
    .lightBlue [ Draw { #color: #lightBlue } ] ]'
```


Now we have two styles: lightGreen and lightBlue that can be applied to any presenter.

### Environmental variables


We can also use environmental variables to get the values of the predefined colors of the current theme, or the fonts. For example, we can create two styles for changing the fonts of the letters of a presenter:

```
'.application [
    .codeFont [ Font { #name: EnvironmentFont(#code) } ],
    .textFont [ Font { #name: EnvironmentFont(#default) } ]
]'
```


Also we can change the styles for all the presenters by default.
We can put by default all the letters in bold.

```
'.application [
	Font { #bold: true }
]'
```


!!todo unclear what is EnvironmentFont vs Font What are the environmental variables


### Defining an Application


To use styles we need to associate the main presenter with an application. The class `SpApplication` already has default styles. To not redefine all the properties for all the presenters, we can concatenate the default styles \(`SpStyle defaultStyleSheet`\) with our own. As said above, the styles are actually STON files that need to be parsed.

To parse the string into a STON we can use the class `SpStyleVariableSTONReader`.

```
presenter := SpPresenter new.
presenter application: (app := SpApplication new).

styleSheet := SpStyle defaultStyleSheet, 
	(SpStyleVariableSTONReader fromString: 
	'.application [
	     Font { #bold: true },
            .lightGreen [ Draw { #color: #B3E6B5 } ],
            .bgBlack [ Draw { #backgroundColor: #black } ],
	    .blue [ Draw { #color: #blue } ]
]' ).

app styleSheet: styleSheet.
```


Now, we can add one or more styles to a presenter, as follows and whose result is shown in Figure *@style5@*.

```
presenter layout: (SpBoxLayout newTopToBottom
	add: (label := presenter newLabel);
	yourself).

label label: 'I am a label'.
label addStyle: 'lightGreen'.
label addStyle: 'black'.

presenter openWithSpec.
```


![Styling.](figures/style5.png width=50&label=style5)

### Dynamically applying styles


We can also remove and add styles at runtime as shown in the following snippet whose result is displayed in Figure *@style6@*.

```
label removeStyle: 'lightGreen'.
label removeStyle: 'bgBlack'.
label addStyle: 'blue'.
```


![After changing the style.](figures/style6.png width=50&label=style6)

### Now using classes

Up until now we just wrote scripts. Now we want to show how we can use style using presenter classes.
To properly use styles, it is better to define a custom application as a subclass of `SpApplication`.


!!todo How do we associate an application to a presenter?

```
SpApplication << #CustomStylesApplication
	slots: {};
	package: 'Spec-workshop'
```


In the class we need to override the method `styleSheet` to return our custom style sheet concatenated with the default one.

```
CustomStylesApplication >> styleSheet

	^ SpStyle defaultStyleSheet, 
		(SpStyleVariableSTONReader fromString:
	'.application [
		Font { #bold: true },
		.lightGreen [ Draw { #color: #B3E6B5 } ],
		.lightBlue [ Draw { #color: #lightBlue } ],
		.container [ Container { #padding: 4, #borderWidth: 2 } ],
		.bgOpaque [ Draw { #backgroundColor: EnvironmentColor(#base) } ],
		.codeFont [ Font { #name: EnvironmentFont(#code) } ],
		.textFont [ Font { #name: EnvironmentFont(#default) } ],
		.bigFontSize [ Font { #size: 20 } ],
		.smallFontSize [ Font { #size: 14 } ],
		.icon [ Geometry { #width: 30 } ],
		.buttonStyle [ Geometry { #width: 110 } ],
		.labelStyle [ 
			Geometry { #height: 25 },
			Font { #size: 12 }	]
	]')
```


We can use different properties in the same style. For example, in `labelStyle` we are setting the height of the presenter to 25 scaled pixels and the font size to 12 scaled pixels. Also, we are using `EnvironmentColor(#base)` for obtaining the default background colour according to the current theme. Because the colour will change according to the theme that used in the image.


### Defining a presenter for the Editor


For the main presenter, we will build a mini-text-viewer in which we will be able to change the size and the font of the text that we are viewing.

```
SpPresenter << #CustomStylesPresenter
	slots: { #text . #label . #zoomOutButton . #textFontButton . #codeFontButton . #zoomInButton };
	package: 'Spec-workshop'
```


In the `initializePresenters` method we will first initialise the presenters, then set the styles for the presenters and finally initialise the layout.

```
CustomStylesPresenter >> initializePresenters

	self instantiatePresenters.
	self initializeStyles.
	self initializeLayout
```


```
CustomStylesPresenter >> instantiatePresenters

	zoomInButton := self newButton.
	zoomInButton icon: (self iconNamed: #glamorousZoomIn).
	zoomOutButton := self newButton.
	zoomOutButton icon: (self iconNamed: #glamorousZoomOut).

	codeFontButton := self newButton.
	codeFontButton
		icon: (self iconNamed: #smallObjects);
		label: 'Code font'.
	textFontButton := self newButton.
	textFontButton
		icon: (self iconNamed: #smallFonts);
		label: 'Text font'.

	text := self newText.
	text
		beNotEditable
		clearSelection;
		text: String loremIpsum.

	label := self newLabel.
	label label: 'Lorem ipsum'
```


```
CustomStylesPresenter >> initializeLayout
	
	self layout: (SpBoxLayout newTopToBottom
		add: label expand: false;
		add: (SpBoxLayout newLeftToRight
			add: textFontButton expand: false;
			add: codeFontButton expand: false;
			addLast: zoomOutButton expand: false;		
			addLast: zoomInButton expand: false;
			yourself)
		expand: false;
		add: text;
		yourself)
```


Finally, we change the window title and size:

```
CustomStylesPresenter>> initializeWindow: aWindowPresenter

	aWindowPresenter
		title: 'Using styles';
		initialExtent: 600 @ 400
```


Without setting the custom styles nor using our custom application in the presenter, we obtain Figure *@style7@*:

![Styling.](figures/style7.png width=70&label=style7)

### Initializing styles


We do not want the black background color for the text presenter. We will like to have a sort of muti-line label. We want the zoom buton to be smaller as they only have icons. We want to have the option to change the size and font of the text inside the text presenter. Finally, why not, we want to change the color of the label, change the height and make it a little bit bigger.

```
CustomStylesPresenter >> initializeStyles

    "Change the height and size of the label and the color as ligthgreen"
    label addStyle: 'labelStyle'.
    label addStyle: 'lightGreen'.

    "The default font of the text will be the code font and 
	the font size will be the small one."
    text addStyle: 'codeFont'.
    text addStyle: 'smallFontSize'.
	
    "Change the background color."
    text addStyle: 'bgOpaque'.

    "But a smaller width for the zoom buttons"
    zoomInButton addStyle: 'icon'.
    zoomOutButton addStyle: 'icon'.
	
    codeFontButton addStyle: 'buttonStyle'.
    textFontButton addStyle: 'buttonStyle'.

    "As this presenter is the container, set to self the container
    style to add a padding and border width."
	
    self addStyle: 'container'
```


![Styled editor.](figures/style8.png width=70&label=style8)

Finally, we have to override the `start` method in the application. With this, we are going to set the application of the presenter and run the presenter from the application.

```
CustomStylesApplication >> start

	(self new: CustomStylesPresenter) openWithSpec
```


Now, when we run `CustomStylesApplication new start` we will obtain Figure *@style8@*.




### Wiring buttons


The only thing missing is to add the behaviour when pressing the buttons.

For example, if we click on the zoom in button we want to remove the `smallFontStyle` and add the `bigFontSize`. When we click on the text font button, we should remove the style codeFont and add the textFont style. 

This is what to do in the `connectPresenters` method defined below:

```
CustomStylesPresenter >> connectPresenters

	zoomInButton action: [
		text removeStyle: 'smallFontSize'.
		text addStyle: 'bigFontSize' ].
	zoomOutButton action: [ 
		text removeStyle: 'bigFontSize'.
		text addStyle: 'smallFontSize'].

	codeFontButton action: [
		text removeStyle: 'textFont'.
		text addStyle: 'codeFont' ].
	textFontButton action: [ 
		text removeStyle: 'codeFont'.
		text addStyle: 'textFont']
```


Now, when we click on zoom in, we obtain Figure *@style9@*.

![Zoomed styled editor.](figures/style9.png width=60&label=style9)

Now, when we select the button text font, we obtain Figure *@style10@*.

![Zoomed styled editor.](figures/style10.png width=60&label=style10)





### Spec implementation details 

You can ask an adapter for its style name using the message `styleName`

```
 SpMorphicLabelAdapter styleName 
 > Label
 ```


### Conclusion


Using styles in Spec is great. It make easier to have a consistent design as we can add the same style to several presenters. If we want to change some style, we only edit the styles sheet. Also, the styles automatically scale if we change the font size of all the image. They are one of the main reason why in Spec we have the notion of an application. We can dynamically change how a presenter looks.
