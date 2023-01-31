## Spec core in a nutshell 
@cha_core 

status: ready for review


Spec is a framework in Pharo for describing user interfaces. 
It allows for the construction of a wide variety of UIs; from small windows with a few buttons up to complex tools like a debugger. 
Indeed most tools in Pharo are written in Spec, e.g., the inspector, spotter, the Pharo debugger, Iceberg, etc. 
In this short chapter we place the key architectural elements of Spec in the context.

### Spec core principle 

The fundamental principle behind Spec is the reuse of user interface logic and visual composition. 
User interfaces are built by reusing and composing existing user interfaces, configuring them as needed. 
This principle starts from the most primitive elements of the UI: widgets such as buttons and labels are in themselves complete UIs that can be reused, configured, and opened in their own window. 
These elements can be combined to form more complex UIs that again can be reused as part of a bigger UI, and so on. 
 
To allow such a reuse, Spec is influenced by the Model View Presenter (MVP) pattern.  
Spec recognizes the need for a Presenter or ApplicationModel class (in Spec represented by the abstract superclass `SpPresenter`) that manages the logic and the link between widgets and domain objects. 
Fundamentally, when writing Spec code, the developer does ‘‘not’’ come into contact with UI widgets, instead one subclass of `SpPresenter` is programmed that holds the UI logic. 
When the UI is opened this model will then instantiate the appropriate widgets. 

Spec offers different backends to render the presenters: Morphic (the default backend) and Gtk. It means than, without modiying your UI described as presenters with Spec, you can render your application in the Pharo image with Morphic or as a native application with external Windows thanks to the Gtk backend. 
 

### Spec architecture overview

Figure *@coreextended@* presents the general architecture of Spec. Basically Spec is built around 5 elements that we will describe in subsequent section. The most important elements are Presenter, Layout and Application. 

A presenter is representing the UI element logic and it is also the connection with the domain.  The Application is also a place to be in contact with domain objects but generally it is handling application specific resources (icons, windows...).

Based on presenters and layout, Spec builds the actual UI. To do so it internally uses adapters that are specific to each widget and per back-end. 
This way presenters are totally agnostic about back-ends and are reusable accross them. 


![Architecture of Spec.](figures/coreExtended.pdf label=coreextended&width=80) 
 

 
### Spec core architecture overview  
 
Spec core is composed of the following elements: 
 
- **Application.** An application is composed of multiple presenters and a stylesheet. 
- **Presenters.** A presenter is a unit of interactive behavior. It is connected to domain objects and other presenters. Its visual representation is defined via at least one layout. 
- **Layout.** A layout describes positions of elements and it can be recursive. 
- **Stylesheet and Styles**. A stylesheet is composed of Styles which describe visual properties such as fonts, colors, .... 
 
 
![Presenter, Application, Layout and Style of Spec.](figures/core.pdf label=core&width=60) 
 
 
We detail now each of the main elements. 
 
 
### Presenters 

A Spec presenter (an instance of `SpPresenter` subclass), is an essential part of the Spec framework. 
It represents the logic of a UI element. It can be the model of a simple UI widget such as a button as well as of a complex UI widget composed by many others presenters (either simple or complex).
To build your User Interface, you compose presenters. 

Spec already comes with a predefined set of basic presenters (widgets) ready to use in your presenters. 
You can find them in scripting - widgets category of the `SpPresenter` class. You will find buttons, labels, check boxes, text input, drop lists, lists, menus, tables, trees, toolbar, action bars, but also more complex widgets like diff presenters or notebooks. 
You can easily instantiate a new presenter and display it: 
 
``` 
SpButtonPresenter new 
	label: 'ok';
	open
```

A presenter may also have a model that is a domain object you need to interact with to display or update data. 
In this case, your presenter class should inherit from `SpPresenterWithModel` so that the presenter keeps a reference to the domain object and gets changed when the model changes (See Chapter *@cha_model@*).

A presenter defines layouts, one is mandatory.
If you want to display a presenter with the default layout, you can use the `open` or `openDialog` methods.
The former will open a new window with the presenter while the later will open a blocking dialog with the presenter.
You can use `openWithLayout:` or `openDialogWithLayout:` to open the presenter with the layout you will provide as argument.


### Application

A spec application (`SpApplication` or one of its subclasses instance) handles your application initialization, configuration, and resources. 
`SpApplication` is not a presenter because it does not has a graphical representation: An `SpApplication` defines your application (keeping the backend, theme, icons, other graphical resources), keeps the flow of windows (and the opened windows that belongs to that application) but it is not shown itself.

It also keeps the windows you have currently opened.
 
A Spec application also provides a way to access windows, to access resources like icons, and provide abstractions for interactions with the user (inform, error, file or directory selection). 
 
 An application also provides the style used by Spec to style UI elements.  
 A default style is available but you can customize it as shown in Chapter *@Style@*. 
 
You should also define a method to tell what is the main window / presenter to use when running the application.
Here we specialize the method `start` as follows: 

``` 
MyApplication >> start
	(MyMainPresenter newApplication: self) open
```

You can run your application with `MyApplication new run`. It will call the `start` method you defined. 


### Application configuration

In the application initialization, you can configure the backend you want to use: morphic (default) or Gtk.

##### Using Morphic

Here is an example using the Film application tutorial. 
We define a configuration as subclass of `SpMorphicConfiguration`.

```language=Smalltalk
SpMorphicConfiguration << #ImdbMorphicConfiguration
	package: 'Spec2-TutorialOne'
```

Then we define the method `configure:` as follows:. 

```language=Smalltalk
ImdbMorphicConfiguration >> configure: anApplication

	super configure: anApplication.
	"There are ways to write/read this from strings or files, but this is how you do 
	 it programatically"
	self styleSheet 
		 addClass: 'header' with: [ :style |
		 	style 
				addPropertyFontWith: [ :font | font bold: true ];
				addPropertyDrawWith: [ :draw | draw color: Color red ] ]
```
Note that we could use a style described in a string as shown in the Style chapter (Chapter *@style@*).

Finally in the corresponding application class we declare that the Morphic back-end should use our configuration 
using the message `useBackend:with:`.

```language=Smalltalk
ImdbApp >> initialize
	super initialize.
	self useBackend: #Morphic with: ImdbMorphicConfiguration new
```


##### Using GTK theme and settings

For Gtk the process is similar, we define a subclass of `SpGtkConfiguration`.

```language=Smalltalk
SpGtkConfiguration << #ImdbGtkConfiguration
	package: 'Spec2-TutorialOne'
```
Then we configure it selecting and extending CSS. 

```language=Smalltalk
ImdbGtkConfiguration >> configure: anApplication

	super configure: anApplication.
	"This will choose the theme 'Sierra-dark' if it is available"
	self installTheme: 'Sierra-dark'.
	"This will add a 'provider' (a stylesheet)"
	self addCSSProviderFromString: '.header {color: red; font-weight: bold}'
```
And iun the application initialization we declare that the configuration should be use for Gtk.

```language=Smalltalk
ImdbApp >> initialize
	super initialize.
	self useBackend: #Gtk with: ImdbGtkConfiguration new
```




### Layouts 

To display its elements, a presenter uses a layout. 
A layout describes how elements are placed on the displayed surface.
To help you build nice user interfaces, several layouts are available: 
 
- **GridLayout**: choose this layout when you need to create a widget with label, fields that needs to be aligned (form-style). You can specify in which box of the grid you want to place an element. 
- **BoxLayout**: a `SpBoxLayout` arranges presenters in a box, vertically (top to bottom) or horizontally \(left to right\). 
- **PanedLayout**: a `SpPanedLayout` is a `BoxLayout` with only 2 elements with a splitter in between, that user can drag to resize the panel. 
- **TabLayout**: a `SpTabLayout` will show all its elements as tabs you can select to display the content. 
- **MillerLayout**: a layout to implement miller columns ([https://en.wikipedia.org/wiki/Miller\_columns](https://en.wikipedia.org/wiki/Miller_columns)), also known as cascading lists. 
 
Any layout in Spec is dynamic and composable. 
In general a layout is define as the presenter instance level, but it can be defined on the class side. 
 
To define a layout is as simple as defining the following `defaultLayout` method. This method is automatically invoked if a layout is not manually set. 
The following method define two box layouts:
- one containing a tree and a list and  
- the second one containing the first one and a code text below.
Each of the layout refers to presenters accessible (`treeClasses`, `methodsFilteringList`, `codeShower`) from the current one.
 
Figure *@layout6B@* shows the corresponding result.

``` 
MyMiniBrowserPresenter >> defaultLayout
    ^ (SpBoxLayout newTopToBottom
        spacing: 5;
        add: (SpBoxLayout newLeftToRight
                spacing: 10;
                add: treeClasses;
                add: methodsFilteringList;
                yourself);
        add: codeShower;
        yourself) 
```

![The layout corresponding to the `defaultLayout` method.](figures/layout6Annotated.pdf width=70&label=layout6B)






### Styles

A Spec application always comes with a default style sheet.  
A style sheet contains style definitions that can be applied to presenters. 
Chapter *@chastyle@* presents styles in detail.
 
A style is a property container to “style” components, and define (in a certain degree) its behaviour within the different layouts implemented.
 
Here is an example of style sheet:
 
```
'.application [       
    .lightGreen [ Draw { #color: #B3E6B5 } ],
    .lightBlue [ Draw { #color: #lightBlue } ] ]'
```


The styles in Spec format are similar to CSS but expressed in STON.
Pay attention not to forget the leading periods. 

You can apply it on your Spec application sending the `styleSheet:` method:

``` 
myStyleSheet := SpStyleVariableSTONReader fromString: 
	'.application [
	    Font { #bold: true },
	.bgBlack [ Draw { #backgroundColor: #black } ],
	    .blue [ Draw { #color: #blue } ]
]'
application styleSheet: SpStyle defaultStyleSheet, myStyleSheet. 
``` 

Then you can style a presenter using the message `addStyle:` as follows:
 
```
presenter label: 'I am a label'.
presenter addStyle: 'blue'. 
```

### Navigation between presenters 
 
 
Once the definition of your UI components (i.e., your Spec presenters and layouts) done, you will need to define the behaviour of the UI: what happens when you open a new presenter? 

You will probably want to provide some data (a model) to the presenter so that it can use to diplay data. 
It is called a transmission: you transmit data from a presenter to another presenter.
Transmissions can be achieved either “manually” by reacting to events or by using the Spec transmission concept.

#### Using presenter events 

It is quite easy to define the behaviour of the UI by using widgets predefined events. 
You can find them in the api-events protocol of the presenters. 
Most used events are `whenSelectionChangedDo:`, `whenSelectedDo:`, `whenModelChangedDo:`, `whenTextChangedDo:`, `action:`. 
Here are some examples: 
 
``` 
messageList 
	whenSelectionChangedDo: [ :selection | 
		messageDetail model: selection selectedItem ];
	whenModelChangedDo: [ self updateTitle ].
	textModel whenSubmitDo: [ :text | self accept: text ].
	addButton action: [ self addDirectory ].
	filterInput whenTextChangedDo: [ :text | self refreshTable ]. 
```

The transmissions using ports are explained in Chapter *@chatransmissions@*.

### Conclusion


As we started to see it, the class `SpPresenter` is a central class which as the following responsibilities:

- Initialization of presenter part and state. 
- Definition of application layout.
- Connection of the elements to support the interaction flow.
- Update of the UI components.

We will illustrate these points in the following chapters.
