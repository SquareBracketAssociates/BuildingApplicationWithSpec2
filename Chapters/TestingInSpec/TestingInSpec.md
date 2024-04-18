## Testing Spec applications
@cha_testing

status: Looking for some source of already written tests - may be the launcher so that people can have a look

Developers often think that testing a user interface is difficult. It is true that fully testing the placement and layout of widgets can be tedious. However, testing the logic of an application and in particular the interaction logic is possible and this is what we will show in this chapter. We show that testing a Spec application is simple and effective.


### Testing presenters

Tests are key to ensuring that everything works correctly. In addition, they free us from the fear of breaking something without being warned about it. Tests support refactorings. While such facts are general and applicable to many domains, they are also true for user interfaces.

#### Spec architecture


Spec is based on an architecture with three different layers as shown in Figure *@fig:Architecture@*:
- **Presenters:** Presenters define the interaction logic and manipulate domain objects. They access backend widgets but via an API that is specified by Adapters.
- **Adapters:** Adapters are objects exposing low-level backend widgets. They are a bridge between presenters and low-level widgets.
- **Backend widgets**. Backend widgets are plain widgets that can be used without Spec.


![Spec Architecture: three layers Presenters - Adapters - Backends.](figures/ArchitectureSpec2.pdf width=95&label=fig:Architecture)

#### Three roles and concerns

To help you understand the different possibilities of testing that you can engage in, we identify the following roles and their related concerns.

- **Spec Users.** Spec users are developers that build a new application. They define the logic of the application by assembling presenters and domain objects. We believe that this is the role that you will play most of the time.
- **Spec Developers.** Spec developers are more concerned with the development of new Spec presenters and their link with the adapters.
- **Widget Developers.** Widget developers are concerned about the logic and working of a given widget for a given backend.

#### Spec user perspective

We will focus on the first role. For the reader interested in the second role, the class `SpAbstractBackendForTest` is a good starting place.

As a Spec user, you should consider that the backends are working and your responsibility is to test the logic of the user interface components.
We should make sure that when the model changes, the user interface components reflect the changes.
Inversely when the user interface components change, we should ensure that the model is updated.
But let us give an example.


### Spec user example


We will test a simple spec application. The model for this application can be any class. It shows all the subclasses of the model in a tree presenter. Also, it has a text presenter that shows the definition string for the selected class. Finally, it has a string morph and a button. When the button is pressed, the color of the morph changes randomly.

![A Spec application.](figures/examplespecapplication.jpg width=70&label=fig:SpecApp)

First, we will define a test class.

```
TestCase << #ClassVisualizerPresenterTest
    slots: { #presenter };
    package: 'Spec-Testing'
```

#### Correct initialization


The tool will be instantiated with a model. In this case, we will use `Object` because it is the root of almost all classes. When we instantiate the spec application of Figure *@fig:SpecApp@*, all the subpresenters of the application must show the data of the model.

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

We have to create a few methods on `ClassVisualizerPresenter` to allow a proper testing.
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
When a class is selected in the tree presenter, the text presenter should change according to the definition of the selected class.
The morph must change as well.

```
ClassVisualizerPresenterTest >> testSelectItemOnTreePresenter

    "We have initialized the tree with Object as its roots.
     The class OrderedCollection is a subclass of Object.
     We would simulate that a user selects OrderedCollection
     from the tree presenter."

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


The action of the color button changes the color of the morph randomly.
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


For this application, we only want the text presenter to show the class definition.
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

#### Checking the window

Now we want to check that the window is built correctly. Here, we will test that the title and the initial extent of the window are correct.

```
testInitializeWindow

    presenter := ClassVisualizerPresenter on: Object.

    [ window := presenter open.
    self assert: window isBuilt.
    self assert: window title equals: 'Class visualizer'.
    self assert: window initialExtent equals: 600 @ 500 ]
        ensure: [ window close ]
```

### Testing your application

In Spec, an application is responsible to run and gather the windows of your application. The pattern is to override the `start` method of your application. The method `start` is a hook method that is invoked when you execute your application using the `run` message as in `MyApplication new run`.

It is important to see that in the `start` method you should configure the presenter you are opening so that it knows its application. This is important so that the application knows the windows it is opening.

```
MyApplication >> start

    MyPresenter new
        application: self;
        open
````

The follwowing code in `tearDown` will automatically close your application windows.

```
MyApplicationTest>>tearDown

	application ifNotNil: [ application closeAllWindows ].
	super tearDown
```

### Known limitations and conclusion

In this chapter we showed that you can take advantage of Spec to define tests that will help you to evolve the visual part of your application. This is really key for modern software development and to lower your stress in the future. So take advantage of agile development.

Currently, Spec does not offer a way to script and control popup windows. It is not possible to script a button that opens a dialog for a value. Future versions of Spec should cover this missing feature.
