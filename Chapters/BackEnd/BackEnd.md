## Using back-ends (Draft)


### Using GTK theme and settings


```language=Smalltalk
SpGtkConfiguration << #ImdbGtkConfiguration
	package: 'Spec2-TutorialOne'
```


```language=Smalltalk
ImdbGtkConfiguration >> configure: anApplication

	super configure: anApplication.
	"This will choose the theme 'Sierra-dark' if it is available"
	self installTheme: 'Sierra-dark'.
	"This will add a 'provider' (a stylesheet)"
	self addCSSProviderFromString: '.header {color: red; font-weight: bold}'
```


```language=Smalltalk
ImdbApp >> initialize
	super initialize.
	self useBackend: #Gtk with: ImdbGtkConfiguration new
```


### Using Morphic


!!todo Explain what we are doing. Whay do we need a configuration. How does it relate to style?

```language=Smalltalk
SpMorphicConfiguration << #ImdbMorphicConfiguration
	package: 'Spec2-TutorialOne'
```


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


```language=Smalltalk
ImdbApp >> initialize
	super initialize.
	self useBackend: #Morphic with: ImdbMorphicConfiguration new
```



### How to run GTK


Hello,

Remember: every time you execute "pharo-ui" you are invoking the UI \(as the last part of the name says\). If you want to NOT have the UI, you need to execute "pharo" :

./pharo Pharo.Image eval RunGtk execute

BUT: this will not work because the image will evaluate "RunGtk execute" and then will exit \(because it will evaluate it as a script\). To avoid that you need to execute: 

./pharo Pharo.Image eval --no-quit "RunGtk execute"

That will work as you want.

BUT, this is not how executing Spec applications is envisaged :

I guess you defined an application \(a children of SpApplication?\) where you set your backend to make it a Gtk application ?
and you have override #start to do something like \(MyPresenter newApplication: self\) openWithSpec ?

In that case, you just need to define in your application class \(say is named MyApplication\) :

MyApplication class >> applicationName
    ^ 'gtkapp'

then, is enough to say:

./pharo Pharo.image run gtkapp

which would be the "canonical" way to do it :\)

Esteban