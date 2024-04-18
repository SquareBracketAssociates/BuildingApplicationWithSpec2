## Traffic Lights 

In this chapter, we will present a traffic light system. 
It is interesting because it shows that we can build different user applications with Spec. 
It illustrates how a spec application can use Morphic objects.

The code and the suggestion for this chapter have been made by Prof. S. Jaroshko from the National University Ivan Franko of Lviv.

![A little traffic ligth simulator.](figures/TrafficLights width=30)

We will start describing the object model and then the presenters. 

### Traffic Lamp

A traffic light is composed of lamps, one of each color. It has an active lamp and a stopwatch.
In addition, it knows its view (a morph). This design does not fully separate the domain object (the traffic lights) from the 


```
Object << #TrafficLights
	slots: {#lamps . #activeLamp . #mustRun . #stopwatch . #morphView};
	package: 'TrafficLightsProject'
```

### Configuration Panel 

We will start creating a panel to control the traffic lights: we want to be able to control the duration of each of the lights.

We want to obtain a UI similar to the one shown in Figure *@input@*.

![An input panel.](figures/InputPanel.png width=30&label=input)


```
SpPresenter << #InputPanelPresenter
	slots: { #redInput . #yellowInput . #greenInput . #readyButton };
	package: 'TrafficLightsProject'
```

```
InputPanelPresenter >> initializePresenters

	redInput := self newNumberInput
			rangeMinimum: 1 maximum: 10;
			number: 5;
			yourself.
	yellowInput := self newNumberInput
			rangeMinimum: 1 maximum: 10;
			number: 2;
			yourself.
	greenInput := self newNumberInput
		rangeMinimum: 1 maximum: 10;
		number: 5;
		yourself.
	readyButton := self newButton
		label: 'Ready';
		icon: (self iconNamed: #smallOk);
		yourself
```

```
InputPanelPresenter >> defaultLayout
	
	^ SpGridLayout new
		add: 'Set durations of ligths' at: 1 @ 1 span: 2 @ 1;
		add: 'red' at: 1 @ 2;
		add: redInput at: 2 @ 2;
		add: 'yellow' at: 1 @ 3;
		add: yellowInput at: 2 @ 3;
		add: 'green' at: 1 @ 4;
		add: greenInput at: 2 @ 4;
		add: readyButton at: 2 @ 5;
		yourself
```

```
InputPanelPresenter >> initializeWindow: aWindowPresenter

	aWindowPresenter
		title: 'Set durations of lights';
		initialExtent: 200 @ 200
```




### Traffic Presenters

```
SpPresenter << #TrafficPresenter
	slots: {#view . #body . #manualButton . #autoButton . #durationPanel . #quitButton . #model};
	package: 'TrafficLightsProject'
```

```
TrafficPresenter >> initializePresenters

	self initializeMorph.
	view := self instantiate: SpMorphPresenter.
	manualButton := self newButton.
	autoButton := self newButton.
	quitButton := self newButton.
	durationPanel := self instantiate: InputPanelPresenter.

	view morph: body.
	manualButton
		label: 'Manual switch';
		icon: (self iconNamed: #smallDoIt).
	autoButton
		label: 'Auto switch';
		icon: (self iconNamed: #tools).
	quitButton
		label: 'Quit';
		icon: (self iconNamed: #smallQuit).
	durationPanel hide.
```



```
initializeWindow: aWindowPresenter

	aWindowPresenter
		title: 'A traffic lights model';
		initialExtent: 450 @ 710
```


### Application

```
SpApplication << #TrafficApplication
	package: 'TrafficLightsProject'
```

```
TrafficApplication >> start

	| window |
	window := (self newPresenter: TrafficPresenter) open.
	window window beUnresizeable; removeExpandBox.
```

### Conclusion

We build a simple yet different kind of application


