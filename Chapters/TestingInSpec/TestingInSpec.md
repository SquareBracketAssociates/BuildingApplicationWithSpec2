## Testing Spec Applications
@cha_testing

status: ready for review. Looking for some source of already written tests - may be the launcher so that people can have a look

Developers often think that testing UI is difficult. This is true that fully testing the placement and layout of widgets can be tedious. 
However, testing the logic of an application and in particular the interaction logic is possible and this is what we will show in this chapter. 
We show that testing Spec application is simple and effective.


### Testing presenters

Tests are key to ensure that everything works correctly. In addition, they free us from the fear to break something without being warned about it. Tests support refactorings. While such facts are general and applicable to many domains, they also true for user interfaces.



#### Spec architecture


Spec is based on an architecture with three different layers as shown in Figure *@fig:Architecture@*: 
- **Presenters:** Presenters defined the interaction logic and manipulate domain objects. They access back-end widgets but via an API that is specified by Adapters.
- **Adapters:** Adapters are objects exposing low-level back-end widgets. They are a bridge between presenters and low-level widgets.
- **Back-end widgets**. Back-end widgets are plain widgets that can be used without Spec.


![Spec Architecture: three layers Presenters - Adapters - Back-ends.](figures/ArchitectureSpec2.pdf width=95&label=fig:Architecture)

#### Three roles and concerns

To help you understand the different possibilities of testing that you can engage, we identify the following roles and their related concerns.

- **Spec Users.**Spec users are developers that build a new application. They define the logic of the application by assembling together presenters and domain objects. We believe that this is in this role that you will play most of the time.
- **Spec Developers.** Spec developers are more concerned with the development of new Spec presenter and their link with the adapter.
- **Widget Developers.** Widget developers are concerned about the logic and working of a given widget is a given back-end.


% +UI elements under test.>file://figures/UI.png|width=75|label=fig:UI+

#### Spec user perspective

We will focus on the first role. For the reader interested in the second role, the class `SpAbstractBackendForTest` is a good starting place.

As a Spec user, you should consider that the back-ends are working and your responsibilities is to test the logic of the user interface components.
We should make sure that when the model changes, the user interface components reflect the changes.
Inversely when the user interface components change, we should ensure that the model is updated.
But let us check an example.


### Spec user example


We will test a simple spec application. The model for this application can be any class.
It shows in a tree presenter all the subclasses of the model. Also, it has a text presenter that shows the definition string for the selected class.
Finally, it has a string morph and a button. When the button is pressed, the color of the morph changes randomly.

![A Spec application.](figures/example_spec_application.jpg width=75&label=fig:SpecApp)

We will first define a test class.
```
TestCase << #ClassVisualizerPresenterTest
	slots: { #presenter };
	package: 'Spec2-Testing'
```

#### Correct initialization


The tool will be instantiated with a model.
In this case, we will use `Object` because it is the root of almost all classes.
So, when we instantiate the spec application of Figure *@fig:SpecApp@*, all the sub presenters of the application must show the data of the model.

```
ClassVisualizerPresenterTest >> testInitialization

	| model |
	model := String.
	presenter := ClassVisualizerPresenter on: model.
	
	self assert: presenter model equals: model.
	self assert: presenter codeText equals: model definitionString.
	self
		assert: presenter stringMorphContent
		equals: model name
```

We will need to create a few methods on ==ClassVisualizerPresenter== to allow a proper testing.
```
ClassVisualizerPresenter >> selectPath: aCollection 
	tree selectPath: aCollection
```

```
ClassVisualizerPresenter >> stringMorphContent
	^ morphPresenter morph contents
```


#### Selection is changing the morph


When selecting a new item in the tree presenter the text presenter and the morph should change.
The tree presenter shows a tree of classes.
When a class of the tree presenter is selected, the text presenter should change according to the definition of the new selected class.
The morph must change as well.

```
ClassVisualizerPresenterTest >> testSelectItemOnTreePresenter

	"We have initialized the tree with Object as its roots. The class OrderedCollection is a subclass of Object. We would simulate that a user selects OrderedCollection from the tree presenter."

	presenter := ClassVisualizerPresenter on: Object.
	
	presenter selectClass: OrderedCollection.
	
	self
		assert: presenter selectedClass
		equals: OrderedCollection.
	self
		assert: presenter codeText
		equals: OrderedCollection definitionString.
	self
		assert: presenter stringMorphContent
		equals: OrderedCollection name
```

```
ClassVisualizerPresenter >> selectClass: aClass 
	tree selectItem: aClass
```

```
ClassVisualizerPresenter >> selectClass: aClass 
	tree selectItem: aClass
```


#### Triggering the button action


The action of the colour button changes the colour of the morph randomly.
When the button is clicked the morph must change its colour.

```
ClassVisualizerPresenterTest >> testButtonChangesMorph

	| previousColor |
	spApplication := ClassVisualizerPresenter on: Object.
	previousColor := spApplication morphPresenter morph color.
	spApplication colorButton click.
	self
		deny: spApplication morphPresenter morph color
		equals: previousColor
```

```
ClassVisualizerPresenter >> clickOnColorButton
	button click
```

```
ClassVisualizerPresenter >> stringMorphColor
	^ morphPresenter morph color
```



#### The text presenter should not be editable


For this application, we only want that the text presenter shows the class definition.
We do not want the user to be able to edit it.

```
testTextPresenterIsNotEditable

	spApplication := ClassVisualizerPresenter on: Object.
	self deny: spApplication textPresenter isEditable
```

```
ClassVisualizerPresenter >> codePresenter
	^ codePresenter
```

#### Checking window

Now we want to check that the window is built correctly.
Here, we will test that the title and the initial extent of the window are correct.
Also, we will test if the window were built correctly.

```
testInitializeWindow

	presenter := ClassVisualizerPresenter on: Object.
	
	[ window := presenter open.
	
	self assert: window isBuilt.
	self assert: window title equals: 'Class visualizer'.
	self assert: window initialExtent equals: 600 @ 500. ]
		ensure: [ window close ]
```

### Known limitations and conclusion

We show in this chapter that you can take advantage of Spec to define tests that will help you to evolve the visual part of your application.
This is really key for modern software development and to lower your stress in the future. 
So take advantage of agile development.

Currently Spec does not offer a way to script and control popup windows. It is not possible to script a button that opens a dialog for a value.
Future versions of Spec20 should cover this missing feature.
