<div align="center"><img src="https://raw.githubusercontent.com/webui-dev/webui-logo/main/webui_240.png" height="200"></div>

# WebUI4Delphi [![Tweet](https://img.shields.io/twitter/url/http/shields.io.svg?style=social)](https://twitter.com/intent/tweet?text=Add%20WebUI4Delphi%20to%20your%20applications%20to%20use%20any%20web%20browser%20as%20a%20GUI%20in%20your%20application&url=https://github.com/salvadordf/WebUI4Delphi&via=briskbard&hashtags=WebUI4Delphi,delphi,webui)
WebUI4Delphi is a [WebUI](https://github.com/webui-dev/webui) wrapper, which allows you to use any web browser as a GUI, with [Delphi](https://www.embarcadero.com/products/delphi/starter) in the backend and HTML5 in the frontend. 

WebUI4Delphi can be used in 64 bit Delphi applications for Windows. Linux and MacOS support needs testing. 

WebUI4Delphi was developed and tested on Delphi 12.0 but it also supports Delphi XE2. 

WebUI4Delphi includes VCL, FireMonkey (FMX) and console examples.

If you need to embed a web browser instead of using the installed web browser then consider using [CEF4Delphi](https://github.com/salvadordf/CEF4Delphi) or [WebView4Delphi](https://github.com/salvadordf/WebView4Delphi). 


## Features

- Fully Independent (*No need for any third-party runtimes*)
- Lightweight & Small memory footprint
- Fast binary communication protocol between WebUI and the browser (*Instead of JSON*)
- Multi-platform & Multi-Browser
- Using private profile for safety
- Original library written in Pure C
- Help insight documentation.
- Help file available.


## Minimal Example

```pas
program Minimal;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils, uWebUI, uWebUIWindow, uWebUITypes;

var
  LWindow : IWebUIWindow;

begin
  try
    WebUI := TWebUI.Create;
    if WebUI.Initialize then
      begin
        LWindow := TWebUIWindow.Create;
        LWindow.Show('<html><head><script src="webui.js"></script></head> Hello World ! </html>');
        WebUI.Wait;
      end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
```

[More examples](https://github.com/salvadordf/WebUI4Delphi/tree/main/demos)


## Text editor

This [text_editor](https://github.com/salvadordf/WebUI4Delphi/tree/main/demos/Console/text_editor) is a lightweight and portable example written in Delphi and JavaScript using WebUI as the GUI.

![text_editor](https://github.com/salvadordf/WebUI4Delphi/assets/17946341/306533de-5885-4bab-9c05-1627ea9b9bc8)


## Installation

* Open the file packages\WebUI4Delphi.dproj.
* Select the Projects→Build all projects menu option.
* Add the source directory to the search path in your applications.
* Add the "FMX" conditional define in your project options. This define is only required for Firemonkey applications.


## Links
* [Developer Forums](https://www.briskbard.com/forum)
* [WebUI project](https://github.com/webui-dev/webui) 
* [WebUI Pascal project for Lazarus](https://github.com/webui-dev/pascal-webui)
* [C API documentation](https://webui.me/docs/#/c_api)

## Support
If you find this project useful, please consider making a donation.

[![paypal](https://www.paypalobjects.com/en_US/i/btn/btn_donateCC_LG.gif)](https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=FTSD2CCGXTD86)

You can also support this project with Patreon.

<a href="https://patreon.com/salvadordf"><img src="https://c5.patreon.com/external/logo/become_a_patron_button.png" alt="Patreon donate button" /></a>

You can also support this project with Liberapay.

<a href="https://liberapay.com/salvadordf/donate"><img alt="Donate using Liberapay" src="https://liberapay.com/assets/widgets/donate.svg"></a>

## Related projects 
* [CEF4Delphi](https://github.com/salvadordf/CEF4Delphi) 
* [WebView4Delphi](https://github.com/salvadordf/WebView4Delphi)
* [PasDoc](https://pasdoc.github.io/)

## Other resources
* [Learn Delphi](https://learndelphi.org/)
* [Essential Pascal by Marco Cantù](https://www.marcocantu.com/epascal/)


Additional: Delphinus-Support
