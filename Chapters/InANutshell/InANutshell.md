## Spec core in a nutshell 
 
@cha_core 
Spec is a framework in Pharo for describing user interfaces. It allows for the construction of a wide variety of UIs; from small windows with a few buttons up to complex tools like a debugger. Indeed most tools in Pharo are written in Spec, e.g., the inspector, spotter, the Pharo debugger, Iceberg, etc. 
 
The fundamental principle behind Spec is the reuse of user interface logic and visual composition. User interfaces are built by reusing and composing existing user interfaces, configuring them as needed. This principle starts from the most primitive elements of the UI: widgets such as buttons and labels are in themselves complete UIs that can be reused, configured, and opened in their own window. These elements can be combined to form more complex UIs that again can be reused as part of a bigger UI, and so on. 
 
To allow such a reuse, Spec is influenced by Dolphin Smalltalks’ Model View Presenter (MVP) pattern.  
Spec recognizes the need for a Presenter or ApplicationModel class (in Spec called SpPresenter) that manages the logic and the link between widgets and domain objects.  
Fundamentally, when writing Spec code, the developer does ‘‘not’’ come into contact with UI widgets, instead a SpPresenter is programmed that holds the UI logic. When the UI is opened this model will then instantiate the appropriate widgets. 
Spec offers different backends to render the presenters: Morphic (the default backend) and Gtk. It means than, without modiying your UI described as presenters with Spec, you can render your application in the Pharo image with Morphic or as a native application with external Windows thanks to the Gtk backend. 
 
Spec is a standard UI framework in Pharo and differs from the other UI frameworks present: Morphic. Morphic is a general-purpose graphics system originally built for the Self language. Morphic provides standard UI widgets, and as such is used by Spec to render UI’s, but can also be used to build any kind of graphical shape and allows for their direct manipulation. Spec is more restricted than Morphic in that it only allows one to build user interfaces for applications that have typical GUI widgets such as buttons, lists et cetera. 
 
### Spec core architecture overview 
 
 
Spec core is composed of the following elements: 
 
- **Application.** An application is composed of multiple presenters and a stylesheet. 
- **Presenters.** A presenter is a unit of interactive behavior. It is connected to domain objects and other presenters. Its visual representation is defined via at least one layout. 
- **Layout.** A layout describe positions of elements and it can be recursive. 
- **Stylesheet and Styles**. A stylesheet is compose of Styles which describe visual properties such as fonts, colors,.... 
 
 
![Presenter, Application, Layout and Style of Spec.](figures/core.pdf label=core&width=60) 
 
 
We detail now each of the main elements. 
 
### Presenters 
 
 
A Spec presenter (an instance of `SpPresenter`), is an essential part of the Spec framework. It represents a UI element. It could be a simple UI widget such as a button as well as a complex UI widget composed of many others presenters (either simple or complex). To build your User Interface, you compose presenters. 
Spec already comes with a predefined set of basic presenters (widgets) ready to use in your presenters. You can find them in scripting - widgets category of the `SpPresenter` class. You will find buttons, labels, check boxes, text input, drop lists, lists, tables, trees, toolbar, action bars, but also more complex widgets like diff presenters or notebooks. 
You can easily instantiate a new presenter and display it: 
 
``` 
SpButtonPresenter new 
	label: 'ok';
	open
``` 
 
![Presenter, Application, Layout, and Style of Spec.](figures/core.pdf label=core&width=60) 

 
To display its elements, a presenter has a layout. A layout describes how elements should be placed on the displayed surface. 
 
A presenter may also have a model that is a domain object you need to interact with to display or update data. In this case, you should inherit from `SpPresenterWithModel` so that the presenter keeps a reference to the domain object. If the domain object is an instance of Model, it is stored as is in the presenter, else a value holder is created to hold the domain object so that you can be notified when the domain object used by the presenter changes. You can then implement the #modelChanged method to refresh your UI when the model changes. 
 
If you do not need to react to model changes, you can simply inherit from `SpPresenter`, override the `setModelBeforeInitialization:` method to set your domain object and use `MyPresenter on: myDomainObject` to instantiate it. 
A presenter also has a reference to its Spec application. 
 
If you want to display a presenter with the default spec, you can use the `open` or `openDialog` methods. The former will open a new window with the presenter while the later will open a blocking dialog with the presenter. You can use `openWithLayout:` or `openDialogWithLayout:` to open the presenter with the spec you will provide as argument \(you can also provide the name of a selector that returns a spec\). 


### Application

A spec application (`SpApplication` instance) handles your application initialization, configuration and resources. It also keeps the windows you have currently opened.
In the application initialization, you can configure the backend you want to use: morphic (default, Gtk).
 
You should also define a method to tell what is the main window / presenter to use when running the application.

``` 
MyApplication >> start
	(MyMainPresenter newApplication: self) open
```


You can run your application with `MyApplication new run`. It will call the `start` method you defined. 
 
A Spec application also provides a way to access windows, to access resources like icons, and provide abstractions for interactions with the user (inform, error, file or directory selection). 
 
An application also provides the style used by Spec to style UI elements.  
A default style is available but you can customize it as shown in Chapter *@Style@*. 


### Layouts 
 
 
A layout describes how elements are placed on the displayed surface. To help you build nice user interfaces, several layouts are available: 
 
- **GridLayout**: choose this layout when you need to create a widget with label, fields that needs to be aligned (form-style). You can specify in which box of the grid you want to place an element. 
- **BoxLayout**: a `SpBoxLayout` arranges presenters in a box, vertically (top to bottom) or horizontally \(left to right\). 
- **PanedLayout**: a `SpPanedLayout` is a `BoxLayout` with only 2 elements with a splitter in between, that user can drag to resize the panel. 
- **TabLayout**: a `SpTabLayout` will show all its elements as tabs you can select to display the content. 
- **MillerLayout**: a layout to implement miller columns ([https://en.wikipedia.org/wiki/Miller\_columns](https://en.wikipedia.org/wiki/Miller_columns)), also known as cascading lists. 
 
Any layout in Spec is dynamic and composable. 
 
To define a layout is as simple as defining the following method and getting it called from the `initializePresenters` method. 
The following method define two box layouts:
- one containing a tree and a list and  
- the second one containing the first one and a code text below.
 
Figure *@layout6B@* shows the corresponding result.
 
 
``` 
MyMiniBrowserPresenter >> defaultLayout
 
    | classesAndMethodsLayout |
    classesAndMethodsLayout := SpBoxLayout newLeftToRight.
    classesAndMethodsLayout
        spacing: 10;
        add: treeClasses;
        add: methodsFilteringList.
    ^ (SpBoxLayout newTopToBottom
        spacing: 5;
        add: classesAndMethodsLayout;
        add: codeShower;
        yourself) 
```

![The layout corresponding to the `defaultLayout` method.](figures/layout6Annotated.pdf width=70&label=layout6B)


### Styles

A Spec application always comes with a default style sheet.  
A style sheet contains style definitions that can be applied to presenters. The chapter *@chastyle@* presents styles in detail.
 
A style is a property container to “style” components, and define (in a certain degree) its behaviour within the different layouts implemented.
 
Here is an example of style sheet:
 
```
'.application [       
    .lightGreen [ Draw { #color: #B3E6B5 } ],
    .lightBlue [ Draw { #color: #lightBlue } ] ]'
```


The styles in Spec format are similar to CSS but expressed in STON.
You can apply it on your Spec application:

``` 
myStyleSheet := SpStyleVariableSTONReader fromString: 
	'.application [
	    Font { #bold: true },
	.bgBlack [ Draw { #backgroundColor: #black } ],
	    .blue [ Draw { #color: #blue } ]
]'
application styleSheet: SpStyle defaultStyleSheet, myStyleSheet. 
``` 
 
 
Then you can style a presenter using the message `addStyle:` as follows 
 
```
presenter label: 'I am a label'.
presenter addStyle: 'blue'. 
```

### Navigation between presenters 
 
 
Once the definition of your UI components (i.e., your Spec presenters and layouts) done, you will need to define the behaviour of the UI: what happens when you open a new presenter? You will probably want to provide some data (a model) to the presenter so that it can use to diplay data. It is called a transmission: you transmit data from a presenter to another presenter.
Transmissions can be achieved either “manually” by reacting to events or by using the Spec transmission concept.

#### Using presenter events 

It is quite easy to define the beahviour of the UI by using widgets predefined events. 
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


### Conclusion


As we started to see it, the class `SpPresenter` is a central class which as the following responsibilities 

- Initializing its part and state 
- Defining the layout of the application 
- Connecting the elements to support the interaction flow 
- Updating the UI components 

We will illustrate these points in the following chapters. 