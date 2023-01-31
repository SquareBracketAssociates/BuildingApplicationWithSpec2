## Tips and how tos  


### Table with sorting  

``` 
SpPresenter << #SpClassPresenter
    slots: { #table };
    package: 'Spec2-TutorialThree' 
```


```
SpClassPresenter >> initializePresenters 
    table := self newTable
        items: self class environment allClasses ;
        addColumn:
            ((SpStringTableColumn title: 'Name' evaluated: #name)
                width: 250;
                yourself);
        addColumn:
            ((SpStringTableColumn title: '#methods' evaluated: #numberOfMethods)
                width: 80;
                yourself);
        beResizable;
        beMultipleSelection;
        yourself.
```

```
SpClassPresenter class >>defaultLayout

    ^ SpBoxLayout newVertical
        add: #table
        yourself 
```

![Ascending table.](figures/FirstClass1-Ascending.png width=80) 


#### Initialize sorting


`sortingBlock: ` defines the default sorting of elements.
Here `#name descending` specifies that the column should be sorted according the name.
 
``` 
SpClassPresenter >> initializePresenters 

   table := self newTable
        items: self class environment allClasses ;
        sortingBlock: #name descending;
        addColumn:
            ((SpStringTableColumn title: 'Name' evaluated: #name)
                width: 250;
                yourself);
        addColumn:
            ((SpStringTableColumn title: '#methods' evaluated: #numberOfMethods)
                width: 80;
                yourself);
        beResizable;
        beMultipleSelection;
        yourself. 
``` 
 
 
![Descending table .](figures/FirstClass2-Descending.png width=80) 
 
 
#### More explicit value computation 
 
``` 
SpClassPresenter >> initializePresenters 

    table := self newTable
            items: (self class environment allClasses first: 100) ;
            sortingBlock: #name descending;
            addColumn:
                ((SpStringTableColumn title: 'Name' evaluated: #name)
                    width: 250;
                    yourself);
            addColumn:
                ((SpStringTableColumn title: '#methods' evaluated: #numberOfMethods)
                    width: 80;
                    yourself);
             addColumn:
                ((SpStringTableColumn 
                        title: '#LOC' 
                        evaluated: [:each | each methods inject: 0 into: [:s :m| s + m linesOfCode]])
                    width: 80;
                    yourself);
            beResizable;
            beMultipleSelection;
            yourself. 
``` 
 
 
By default, if you do not set `sortFunction: `it will compare what the evaluation block returns. 
 
 
![With more explicit item value computation.](figures/FirstClass3-LOC.png width=80) 
 
#### Specific column-level sorting 
 
The message `sortFunction:` lets us define how a specific column wants to be sorted. 
Here `sortFunction: [ :cl | cl allSubclasses size ] ascending` is telling that the subclass column should be sorted using the number of subclasses. 
 
``` 
SpClassPresenter >> initializePresenters 

    table := self newTable
            items: self class environment allClasses ;
              sortingBlock: #name descending;
            addColumn:
                ((SpStringTableColumn title: 'Name' evaluated: #name)
                    width: 250;
                    yourself);
                addColumn:
                ((SpStringTableColumn title: '#subclass' evaluated: [:each | each allSubclasses size])
                   sortFunction: [ :cl | cl allSubclasses size ] ascending;
                    width: 250;
                    yourself);

            addColumn:
                ((SpStringTableColumn title: '#methods' evaluated: #numberOfMethods)
                    width: 80;
                    yourself);
            beResizable;
            beMultipleSelection;
            yourself. 
``` 
 
 
![With subclasses.](figures/FirstClass4-Subclasses.png width=80) 
![Sorted based on subclass number.](figures/FirstClass5-SubclassesSorted.png width=80) 
 
 
Note that previous example shows that the current implementation forces us to recompute the value while it is not needed. 
 
### Command reuse 
 
With commander2 you can reuse commands. The same commander class can return command instance configured differently. 
 
A simple  
``` 
asSpecCommand
    ...
    iconProvider: self 
``` 
 
 
``` 
iconNamed: aSymbol
    ^ (super iconNamed: aSymbol) asGrey 
```

### Alternate color 

```
    whenBuiltDo: [ :tree| tree widget alternateRowsColor ]  
```



### How to use your values to initialize your widgets 


The `initializePresenters` hook method is called in the `initialize` phase of the presenter. 
It means that you may have problem to pass the values of your contructors before.  
Your code may be as follows:  

```
PRExportPresenter class >> withParser: aParser
    ^ self new
        setParser: aParser;
        yourself 
```


The problem with above is that your `initializeWidgets` method may want to use the value passed \(`withParser:`\) when  
creating your presenter 
 
For example something like that:  
``` 
PRExportPresenter >> initializePresenters

    syntaxScreen := self newLabel label: 'Input syntax: ', self parser name. 
``` 
 
 
If you want to use your values you have to pass them before initialize is called y 
several ways for that 
 
#### Define your own initialize method. 
 
 
Write your own initialize method and set your values before calling the `initialize`. 
 
``` 
PRExportPresenter class >> withParser: aParser
    ^ self basicnew
        setParser: aParser;
        initialize; 
        yourself 
``` 
 
 
This is not really nice because if you forget to call `initialize` you may break Spec2 logic.  
A better solution is the following one. 
 
#### Use on: and setModelBeforeInitialization:

Pass your model to the presenter (this is a single object) when you create your presenter calling `on:` instead `new`  
and you need to implement the method `setModelBeforeInitialization:`. 
 
``` 
PRExportPresenter class >> withParser: aParser
    ^ self on: aParser 
```

```
PRExportPresenter >> setModelBeforeInitialization: aParser
    self setParser: aParser 
```
 
 
### Roassal inside 


```
SpMorphPresenter new
    morph: view canvas createMorph;
    yourself 
```

### How to load icons? 
 
ThemeIcons >> loadIconsFromUrl.
Here's a simplistic approach to load icons from PNGs in a local ZIP archive 

```
| themeIcons newIcons |
newIcons := IdentityDictionary new.
themeIcons := ThemeIcons new.

((FileSystem zip: 'C:\icons\a.zip' asFileReference) 
    open workingDirectory allChildrenMatching: '*.png') do:
        [ :each | newIcons 
            at: each base asSymbol
            put: (themeIcons readPNGFrom: each )].
newIcons. 
```

The class ThemeIcons offers more support.

Another way that is a bit more low-level is to use the `ImageForm` directly. 
 
``` 
    | newIcons |
    newIcons := IdentityDictionary new.
    ((FileSystem zip: 'C:\icons\a.zip' asFileReference) 
        open workingDirectory allChildrenMatching: '*.png') do:
            [ :each | newIcons 
                at: each base asSymbol
                put: (ImageForm open: each )].

    SpToolBarButton new 
       label: 'Go!';
       icon: (newIcons at: #iconFileName). 
```

### Can we draw custom widgets with spec 

You can. You have two possibilities:
- you do a morph and you use `SpMorphPresenter` 
- you draw it with athens and you use `SpAthensPresenter` 
 
Maybe (2) needs some work to be really usable, it was never tried. Also, it needs a rename to `SpAthensPresenter` 


### Migrating from Spec1  
Here is a little list of points that you should consider when you migrate your applications from Spec1 to Spec2. 
 
 
##### Picking the right superclass 

Your presenter or interaction model should now inherit from `SpPresenter` or `SpPresenterWithModel` and not from `ComposablePresenter`.
Note that `SpPresenterWithModel` avoids the definition of the method `setModelBeforeInitialization:`.

##### Definition hook renaming 

Some of the hook methods got renamed to convey better what they are doing: 

- `initializeWidgets` is renamed to `initializePresenters` 
- `initializePresenter` is renamed to `connectPresenters` 

There is not need to define a `defaultSpec` class side method. You can now simply define an `initializeLayout` method and call it. 
The method `defaultSpec` is now called `defaultLayout`.


##### Layouts 

`SpecLayout composed newColumn:` and `newRow:` are now replaced by `SpBoxLayout newTopToBottom` and `newLeftToRight` messages. 
Here you see an example of conversion:
 
``` 
SpecLayout composed
	newColumn: [ : column | 
		column
			newRow: [ :nameRow |
				nameRow 
					add: #nameLabel width: self labelSize;
					add: #nameInputField ]
			height: self inputTextHeight;
			newRow: [ :versionRow |
				versionRow 
					add: #versionLabel width: self labelSize;
					add: #versionInputField ]
			height: self inputTextHeight] 
``` 

``` 
SpBoxLayout newTopToBottom 
	add: (SpBoxLayout newLeftToRight
			add: #nameLabel expand: false;
			add: #nameInputField expand: false;
			yourself);
	add: (SpBoxLayout newLeftToRight
			add: #versionLabel expand: false;
			add: #versionInputField expand: false;
			yourself);
	yourself 
``` 

Use #add:expand: to add items to a layout.
 
##### Others  
  
- You do not need setters and getters to hold subpresenters anymore, so you can remove them.
- You can remove static labels by just declaring them in the layout (e.g. `layout add: 'Name:' expand: false`).
- Panels are replaced by dynamic layouts.