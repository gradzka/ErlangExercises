/* Based on: https://freshman.tech/calculator/ */
const calculator = {
    displayValue: '0',
    firstOperand: null,
    waitingForSecondOperand: false,
    operator: null,
  };

const error = 0
const info = 1
const success = 2
const warning = 3

const performCalculation = 
{
    '/': (firstOperand, secondOperand) => send_query("divi", secondOperand),
  
    '*': (firstOperand, secondOperand) => send_query("mult", secondOperand),
  
    '+': (firstOperand, secondOperand) => send_query("plus", secondOperand),
  
    '-': (firstOperand, secondOperand) => send_query("minus", secondOperand),
  
    '=': (firstOperand, secondOperand) => send_query("eq", 0),

    'inverse':(firstOperand, secondOperand) => send_query("inv", 0),

    'radic':(firstOperand, secondOperand) => send_query("sqrt", 0)
  };

var websocket;

function connect() 
{
    wsHost = "ws://10.160.37.157:8080/websocket";
    websocket = new WebSocket(wsHost);	
    show_on_screen('Connecting to: ' +  wsHost, info);

    websocket.onopen = function(evt) 
    { 
        onopen(evt) 
    }; 

    websocket.onclose = function(evt) 
    { 
        onclose(evt) 
    }; 

    websocket.onmessage = function(evt) 
    { 
        onmessage(evt) 
    }; 

    websocket.onerror = function(evt) 
    { 
        onerror(evt) 
    }; 				
};

function disconnect() 
{
    websocket.close();
};

function handleOperator(nextOperator) 
{
    const 
    { 
        firstOperand, 
        displayValue, 
        operator 
    } = calculator
    const inputValue = parseFloat(displayValue);
  
    if (operator && calculator.waitingForSecondOperand && nextOperator != 'radic' && nextOperator != 'inverse')  
    {
        calculator.operator = nextOperator;
        return;
    }
  
    if (firstOperand == null) 
    {
        calculator.firstOperand = inputValue;
        send_query("set", inputValue)
    } 
    else if (operator && operator != 'radic' && operator != 'inverse' && (((nextOperator == 'radic' || nextOperator == 'inverse') && !calculator.waitingForSecondOperand) || (nextOperator == '+' || nextOperator == '-' || nextOperator == '*' || nextOperator == '/' || nextOperator == '=') ) )
    {
        const currentValue = firstOperand || 0;
        performCalculation[operator](currentValue, inputValue);
    }
  
    if (nextOperator == 'radic' || nextOperator == 'inverse')
    {
        performCalculation[nextOperator](calculator.firstOperand, 0);
    }
    else
    {
        calculator.waitingForSecondOperand = true;
    }
    calculator.operator = nextOperator;
}

function inputDecimal(dot) 
{
    if (calculator.waitingForSecondOperand === true) 
    {
        return;
    }
    
    // If the `displayValue` does not contain a decimal point
    if (!calculator.displayValue.includes(dot)) 
    {
        // Append the decimal point
        calculator.displayValue += dot;
    }
}

function inputDigit(digit) 
{
    const 
    { 
        displayValue, 
        waitingForSecondOperand 
    } = calculator;

    if (waitingForSecondOperand === true || calculator.operator == "radic" || calculator.operator == "inverse") 
    {
      calculator.displayValue = digit;
      calculator.waitingForSecondOperand = false;
      if (calculator.operator == "radic" || calculator.operator == "inverse")
      {
        calculator.firstOperand = null;
      }
    }
    else 
    {
      calculator.displayValue = displayValue === '0' ? digit : displayValue + digit;
    }
}

function load_settings()
{
    const keys = document.querySelector('.calculator-keys');
    keys.addEventListener('click', (event) => 
    {
        const { target } = event;

        if (target.classList.contains('all-clear')) 
        {
            resetCalculator();
            send_query("ac", 0)
            return;
        }

        if (!target.matches('button')) 
        {
            return;
        }

        if (target.classList.contains('decimal')) 
        {
            inputDecimal(target.value);
            updateDisplay();
            return;
        }
    
        if (target.classList.contains('operator')) 
        {
            handleOperator(target.value);
            updateDisplay();
            return;
        }
    
        inputDigit(target.value);
        updateDisplay();
    });

    if (!("WebSocket" in window))
    {
        show_on_screen('Websockets are not supported', warning)
        $("calculator").hide(); 					
    } 
    else 
    {
        show_on_screen('Websockets are supported', info);
        connect();
    };
    $("#output").hide();
}

function onclose(evt) 
{
    show_on_screen('Web socket is disconnected', info);
};

function onerror(evt) 
{
    show_on_screen('Error: ' + evt.data, error);
};

function onmessage(evt) 
{
    calculator.displayValue = evt.data;
    if (calculator.operator!=null)
    {
        calculator.firstOperand = parseFloat(evt.data);
    } 
    updateDisplay(); 
}; 

function onopen(evt) 
{
    show_on_screen('Web socket is connected', success); 			
};

function resetCalculator() 
{
    calculator.displayValue = '0';
    calculator.firstOperand = null;
    calculator.waitingForSecondOperand = false;
    calculator.operator = null;
}

function send_query(operator, secondOperand) 
{
    if (websocket.readyState == websocket.OPEN)
    {
        websocket.send("op=" + operator + "&arg=" + String(secondOperand));
        
        if (operator == 'radic' || operator == 'inverse')
        {
            calculator.operator = null;
        }
    } 
    else 
    {
        show_on_screen('Websocket is not connected', success); 
    };
};

function show_on_screen(txt, alert) 
{   
    $('#status').html('<b>'+ txt +'<b>');
    $('#status').removeClass()

    if (alert == error)
    {
        $('#status').addClass("alert alert-error")
    }
    else if (alert == info)
    {
        $('#status').addClass("alert alert-info")
    }
    else if (alert == success)
    {
        $('#status').addClass("alert alert-success")
    }
    else if (alert == warning)
    {
        $('#status').addClass("alert alert-warning")
    }
};

function updateDisplay() 
{
    const display = document.querySelector('.calculator-screen');
    display.value = calculator.displayValue.substring(0,12);
}

window.onload=load_settings