#include <SoftwareSerial.h>
#include <EEPROM.h>

const byte ledPin = 13;
const byte redPin = A5;
const byte greenPin = A3;
const byte bluePin = A4;
const byte softRxTXPin = A0;

// Ugly c++ 
SoftwareSerial softSerial_0(softRxTXPin + 0, softRxTXPin + 0);
SoftwareSerial softSerial_1(softRxTXPin + 1, softRxTXPin + 1);
SoftwareSerial softSerial_2(softRxTXPin + 2, softRxTXPin + 2);
SoftwareSerial softSerial_3(softRxTXPin + 3, softRxTXPin + 3);
SoftwareSerial softSerial_4(softRxTXPin + 4, softRxTXPin + 4);

SoftwareSerial* softSerial_array[5] = {&softSerial_0, &softSerial_1, &softSerial_2, &softSerial_3, &softSerial_4};
byte active_softSerial;
unsigned long lastcom_softSerial_table[5];

int8_t state, softState;
byte b;
byte buf[4], softbuf[4];
byte EEPROMAdress;

byte blinkLedState = 0;
byte blinkLedFlag = 0;
unsigned long previousMillis = 0;
const long interval = 200;

void set_active_softSerial(byte new_index)
{
  if (new_index >= 5) return;
  active_softSerial = new_index;
  softSerial_array[active_softSerial]->listen();
  pinMode(softRxTXPin + active_softSerial - 1, INPUT_PULLUP);
}


inline byte to_low_case(byte c)
{
  if (c >='A' && c <='Z')
    return  c - 'A' + 'a';
  else 
    return  c;
}

inline byte to_up_case(byte c)
{
  if (c >='a' && c <='z')
    return  c - 'a' + 'A';
  else 
    return  c;
}


void setup() 
{ 
  byte i;
  state = -1;
  softState = -1;
  
  Serial.begin(19200);
  serialSend(0, 'X', EEPROM.read(0));
  serialSend(0, 'Y', EEPROM.read(1));
  
  pinMode(ledPin, OUTPUT);
  // set the data rate for the SoftwareSerial port
  for (i = 0; i < 5; i++){
    softSerial_array[i]->begin(38400);
    pinMode(softRxTXPin + i, INPUT_PULLUP);
  }
  set_active_softSerial(0);
  
  if (EEPROM.read(0) > 0)
    box_setup();
  else if (EEPROM.read(1) > 0)
    machine_setup(); 
  
} 
 
void machine_setup(void)
{
   
}


void box_setup(void)
{
  pinMode(redPin, OUTPUT);
  pinMode(greenPin, OUTPUT);
  pinMode(bluePin, OUTPUT);

  digitalWrite(redPin, LOW);
  digitalWrite(greenPin, LOW);
  digitalWrite(bluePin, LOW);

  digitalWrite(redPin, HIGH);
  delay(250);
  digitalWrite(redPin, LOW);
  delay(250);
  digitalWrite(greenPin, HIGH);
  delay(250);
  digitalWrite(greenPin, LOW);
  delay(250);
  digitalWrite(bluePin, HIGH);
  delay(250);
  //digitalWrite(bluePin, LOW);
}
 
 
byte hexValue(byte hexdigit)
{
  if (hexdigit >= '0' && hexdigit <= '9')
    return hexdigit - '0';
  if ( (hexdigit | 0x20) >= 'a' && (hexdigit | 0x20) <= 'f')
    return (hexdigit | 0x20) - 'a' + 10;
  return 0;
}


void serialSend(byte port, char channel, byte b)
{
  byte i;
  if (port == 0){
    Serial.write(channel);
    Serial.write(hexNibble(b >> 4));
    Serial.write(hexNibble(b));
  } else if (port >= 1 && port <=5){
    i = port - 1;
    pinMode(softRxTXPin + i, OUTPUT);
    //softSerial_array[i]->write(0x55);
    softSerial_array[i]->write(channel);
    softSerial_array[i]->write(hexNibble(b >> 4));
    softSerial_array[i]->write(hexNibble(b));
    pinMode(softRxTXPin + i, INPUT_PULLUP);
  }
}

void processPacket(byte *data, byte port)
{
  byte value, p, v;
  unsigned long ul;

  value = (hexValue(data[1]) << 4) | hexValue(data[2]);
  
  if (port > 0 && port <= 5)
    lastcom_softSerial_table[port -1] = millis();

  // Lower case commands from the hard Serial are sent to the active softSerial
  if (port == 0 && data[0] > 'f' && data[0] <= 'z') {
    //serialSend(1 + active_softSerial, data[0] & ~0x20, value); // To upper case
    serialSend(1 + active_softSerial, data[0], value);
    return;
  // Upper case commands from the Softserial are converted to lowercase and sent to the hard Serial
  } else if (port > 0 && data[0] > 'F' && data[0] <= 'Z') {
    serialSend(0, to_low_case(data[0]), value); // To Lower case
    return;
  } 
  
  switch (to_up_case(data[0])) {
    case 'S': pinMode(softRxTXPin + active_softSerial, OUTPUT);
              softSerial_array[active_softSerial]->write(value); 
              pinMode(softRxTXPin + active_softSerial, INPUT_PULLUP);
              break;

    case 'H': Serial.write(value); 
              break;
              
    case 'R': EEPROMAdress = value; 
              break;
    case 'W': EEPROM.write(EEPROMAdress, value); 
              break;
    case 'V': serialSend(port, 'V', EEPROM.read(value)); 
              break;
    // Ping          
    case 'G': serialSend(port, 'G', value);
              break;
    // LED Read
    case 'T': serialSend(port, 'T', (digitalRead(redPin) << 2)  | 
                                    (digitalRead(greenPin) << 1) | 
                                     digitalRead(bluePin)); 
              break;

    // LED write
    case 'L': digitalWrite(redPin,   (value & 0x04) > 0);
              digitalWrite(greenPin, (value & 0x02) > 0);
              digitalWrite(bluePin,  (value & 0x01) > 0);
              break;

    // Pin Output write
    case 'O': p = value & 0x0F;
              if (p > 13 || p < 3) break; // only pins 3..13
              v = (value & 0x10) > 0; 
              digitalWrite(p, v);
              break;

    // Pin direction write
    case 'P': p = value & 0x0F;
              if (p > 13 || p < 3) break; // only pins 3..13
              v = (value & 0x10); 
              if (v == 0)
                pinMode(p, INPUT);
              else if (v == 1) 
                pinMode(p, OUTPUT);
              else if (v == 2) 
                pinMode(p, INPUT_PULLUP);
              break;

    case 'M': if (value >= 0 && value < 5)
                set_active_softSerial(value);
              break;
    case 'N': serialSend(port, 'N', active_softSerial);
              break;

    case 'K': blinkLedFlag = value;
              break;
    // 
    case 'U': ul = (millis() - lastcom_softSerial_table[value]) / 100;
              if (ul > 255) 
                v = 255;
              else 
                v = ul;
              serialSend(port, 'U', v);
              break;
  }
}


byte hexNibble(byte b)
{
  b = b & 15;
  if (b < 10){
    return b + '0';
  } else {
    return b - 10 + 'A';
  }
}
  
  
byte isHexNibble(char c)
{
  if ((c >= '0' && c <= '9') || ((c | 0x20) >= 'a' && (c | 0x20) <= 'f')) return 1;
  else return 0;
}  
  
void loop() 
{ 
  if (Serial.available()){
    b = Serial.read();
    // A char between g and z is a frame start 
    if ((b | 0x20) > 'f' && (b | 0x20) <= 'z') {
      digitalWrite(ledPin, HIGH);
      state = 0;
      buf[0] = b; // This is the channel
    } else if (state >= 0) { // Waiting for data
      if (isHexNibble(b)) {
        state++;
        buf[state] = b;
      } else { // Bad Packet: restart
        state = -1;
      }
      if (state >= 2) {
        processPacket(buf, 0);
        digitalWrite(ledPin, LOW);
        state = -1;
      }
    }  
  }
  
  if (softSerial_array[active_softSerial]->available()) {
    b = softSerial_array[active_softSerial]->read();
    //Serial.println(b);
    //Serial.write(b);
    if ((b | 0x20) > 'f' && (b | 0x20) <= 'z') {
      softState = 0;
      softbuf[0] = b; // This is the channel
    } else if (softState >= 0) { // Waiting for data
      if (isHexNibble(b)) {
        softState++;
        softbuf[softState] = b;
      } else { // Bad Packet: restart
        softState = -1;
      }
      if (softState >= 2) {
        processPacket(softbuf, 1 + active_softSerial);
        softState = -1;
      }
    }  
  }

  
  unsigned long currentMillis = millis();
  
  if(currentMillis - previousMillis >= interval) {
    // save the last time you blinked the LED 
    previousMillis = currentMillis;   

    // if the LED is off turn it on and vice-versa:
    if (blinkLedState == 0)
      blinkLedState = 1;
    else
      blinkLedState = 0;

    // set the LED with the ledState of the variable:
    if(blinkLedFlag)
      digitalWrite(ledPin, blinkLedState);
  }  
}


