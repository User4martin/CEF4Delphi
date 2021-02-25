// ************************************************************************
// ***************************** CEF4Delphi *******************************
// ************************************************************************
//
// CEF4Delphi is based on DCEF3 which uses CEF to embed a chromium-based
// browser in Delphi applications.
//
// The original license of DCEF3 still applies to CEF4Delphi.
//
// For more information about CEF4Delphi visit :
//         https://www.briskbard.com/index.php?lang=en&pageid=cef
//
//        Copyright © 2021 Salvador Diaz Fau. All rights reserved.
//
// ************************************************************************
// ************ vvvv Original license and comments below vvvv *************
// ************************************************************************
(*
 *                       Delphi Chromium Embedded 3
 *
 * Usage allowed under the restrictions of the Lesser GNU General Public License
 * or alternatively the restrictions of the Mozilla Public License 1.1
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
 * the specific language governing rights and limitations under the License.
 *
 * Unit owner : Henri Gourvest <hgourvest@gmail.com>
 * Web site   : http://www.progdigy.com
 * Repository : http://code.google.com/p/delphichromiumembedded/
 * Group      : http://groups.google.com/group/delphichromiumembedded
 *
 * Embarcadero Technologies, Inc is not permitted to use or redistribute
 * this source code without explicit permission.
 *
 *)

unit uCEFLazarusBrowserWindow;

{$mode objfpc}{$H+}
{$i cef.inc}

interface

uses
  {$IFDEF FPC}
  LResources,
  {$ENDIF}
  uCEFApplication, uCEFChromiumWindow, uCEFTypes, uCEFInterfaces, uCEFChromium,
  uCEFLinkedWinControlBase, Forms, ExtCtrls, LazLogger, Classes, sysutils;

type

  (* On cocoa closing a browser (without closing its top-level window)
     does not work, while the application is in any other event.
     I.e. if the App is in a button-press event, then the browser will
     only close once that event was finished.
     Yet the user-code may have called TLazarusBrowserWindow, which can
     not be cancelled. In that case TLazarusBrowserWindow will be closed,
     and the TChromium kept, until it can close.
     On Mac this is possible, as the containing NSView can be closed before
     the TChromium. On other OS this order may or may not work.

     Once the Application.MainForm closes (app exits) it is possible to wait
     for the TChromium. Even if the terminate was triggered in an event.
     By setting the component owner to the MainForm we can make sure it will
     be called before the app terminates.
  *)

  TLazarusBrowserWindow = class;

  { TChromiumWrapper }

  TChromiumWrapper = class(TComponent)
    protected type
      TWrapperChromiumState   = (csNoBrowser, csCreatingBrowser, csHasBrowser, csClosingBrowser, csCloseAfterCreate);
      TWrapperState           = (wsNone, wsWaitingForClose, wsSentCloseEventAfterWait, wsDestroyAfterWait);
    protected
      FChromium        : TChromium;
      FChromiumState   : TWrapperChromiumState;
      FWrapperState    : TWrapperState;

      FBrowserWindow   : TLazarusBrowserWindow;
      FLoadUrl         : ustring;

      procedure WebBrowser_OnAfterCreated(Sender: TObject; const browser: ICefBrowser);
      procedure WebBrowser_OnClose(Sender: TObject; const browser: ICefBrowser; var aAction : TCefCloseBrowserAction); reintroduce;
      procedure WebBrowser_OnBeforeClose(Sender: TObject; const browser: ICefBrowser); reintroduce;
      {$IFDEF FPC}
      procedure   WebBrowser_OnGotFocus(Sender: TObject; const browser: ICefBrowser);
      {$ENDIF}
      procedure   DoCreated(Data: PtrInt);

      procedure   MaybeDestroy;
    public
      constructor Create(AOwner: TLazarusBrowserWindow); reintroduce;
      destructor  Destroy; override;

      function    CreateBrowser: boolean;
      procedure   LoadURL(aURL: ustring);
      procedure   CloseBrowser(aForceClose: boolean);
      function    IsClosed: boolean;
      (* WaitForBrowserClosed calls ProcessMessages.
         It therefore is possible that the TLazarusBrowserWindow will be destroyed
         when this method returns.
         It is the callers responsibility to take any necessary precaution.
      *)
      procedure   WaitForBrowserClosed;
  end;

  { TLazarusBrowserWindow }

  TLazarusBrowserWindow = class(TCEFLinkedWinControlBase)
    private
      FChromiumWrapper  : TChromiumWrapper;
      FCanClose         : boolean;  // Set to True in TChromium.OnBeforeClose

      FOnBrowserClosed  : TNotifyEvent;
      FOnBrowserCreated : TNotifyEvent;
      FTimer            : TTimer;

      procedure   DoCreateBrowser(Sender: TObject);
    protected
      function    GetChromium: TChromium; override;
      procedure   DestroyHandle; override;
      procedure   RealizeBounds; override;

      procedure   DoEnter; override;
      procedure   DoExit; override;
      procedure   DoOnCreated;
      procedure   DoOnClosed(Data: PtrInt);
      procedure   DoOnFocus(Data: PtrInt);
    public
      constructor Create(AOwner: TComponent); override;
      destructor  Destroy; override;
      procedure   CreateHandle; override;

      procedure   CloseBrowser(aForceClose: boolean);
      procedure   WaitForBrowserClosed;
      function    IsClosed: boolean;
      procedure   LoadURL(aURL: ustring);

    published
      property    Chromium; //        : TChromium    read GetChromium;

      property    OnBrowserCreated : TNotifyEvent read FOnBrowserCreated write FOnBrowserCreated;
      (* OnBrowserClosed will not be called, if the TLazarusBrowserWindow is
         destroyed/destroying before the browser is closed.
      *)
      property    OnBrowserClosed  : TNotifyEvent read FOnBrowserClosed write FOnBrowserClosed;
  end;

  function HasClosingBrowsersWindows: Boolean;

{$IFDEF FPC}
procedure Register;
{$ENDIF}

implementation

function dbgs(s: TChromiumWrapper.TWrapperChromiumState): string;overload;
begin
  writestr(Result, s);
end;

{ TChromiumWrapper }

procedure TChromiumWrapper.WebBrowser_OnAfterCreated(Sender: TObject;
  const browser: ICefBrowser);
begin
  DebugLn(['+++ OnAFterCreate ', dbgs(FChromiumState), ' HasWin:',dbgs(FBrowserWindow<>nil), '    ', ptruint(self), FChromium.Initialized, ',',AnsiString( FLoadUrl)]);

  (* We may still be in Chromium.CreateBrowserSync
     In that case initialization will happen after this event,
     but before the call to CreateBrowser returns
  *)
  Application.QueueAsyncCall(@DoCreated, 0);
end;

procedure TChromiumWrapper.WebBrowser_OnClose(Sender: TObject;
  const browser: ICefBrowser; var aAction: TCefCloseBrowserAction);
begin
  DebugLnEnter(['>>> OnCLOSE --- ', dbgs(FChromiumState), ' HasWin:',dbgs(FBrowserWindow<>nil), '    ', ptruint(self)]);
  (* FBrowserWindow should always be <> nil
     If FBrowserWindow is nil (MacOS) then the FBrowserWindow.Handle is destroyed too,
     and CEF should call BeforeClose, without calling DoClose
  *)
  if (FBrowserWindow <> nil) and FBrowserWindow.DestroyChildWindow then
    aAction := cbaDelay
  else
    aAction := cbaClose;
  DebugLnExit(['>>> OnCLOSE --- ', dbgs(FChromiumState), ' HasWin:',dbgs(FBrowserWindow<>nil), '    ', ptruint(self)]);
end;

procedure TChromiumWrapper.WebBrowser_OnBeforeClose(Sender: TObject;
  const browser: ICefBrowser);
begin
  DebugLnEnter(['>>> On-BEFORE-CLOSE --- ', dbgs(FChromiumState), ' HasWin:',dbgs(FBrowserWindow<>nil), '    ', ptruint(self)]);
  FChromiumState := csNoBrowser;

  if (FBrowserWindow <> nil) then begin
    if FWrapperState = wsWaitingForClose then
      FWrapperState := wsSentCloseEventAfterWait
    else
      Application.QueueAsyncCall(@FBrowserWindow.DoOnClosed, 0);
  end;
  DebugLnExit(['<<< On-BEFORE-CLOSE --- ', dbgs(FChromiumState), ' HasWin:',dbgs(FBrowserWindow<>nil), '    ', ptruint(self)]);
end;

procedure TChromiumWrapper.WebBrowser_OnGotFocus(Sender: TObject;
  const browser: ICefBrowser);
begin
  if (FBrowserWindow <> nil) then
    Application.QueueAsyncCall(@FBrowserWindow.DoOnFocus, 0);
end;

procedure TChromiumWrapper.DoCreated(Data: PtrInt);
begin
  DebugLn(['+++ DoCreated ', dbgs(FChromiumState), ' HasWin:',dbgs(FBrowserWindow<>nil), '    ', ptruint(self), FChromium.Initialized, ',',AnsiString( FLoadUrl)]);

  // Any other state, means this is a late async call
  case FChromiumState of
    csCreatingBrowser: begin
        FChromiumState := csHasBrowser;
        if  FLoadUrl <> '' then
          LoadURL(FLoadUrl);

        if (FBrowserWindow <> nil) then
          FBrowserWindow.DoOnCreated;
      end;
    csCloseAfterCreate: begin
        FChromiumState := csHasBrowser;
        CloseBrowser(True);
      end;
  end;
end;

procedure TChromiumWrapper.MaybeDestroy;
begin
  DebugLnEnter(['>> MaybeDestroy ', dbgs(FChromiumState), ' HasWin:',dbgs(FBrowserWindow<>nil), '    ', ptruint(self)]);
  CloseBrowser(True);
  FBrowserWindow := nil;
  DebugLnExit(['<< MaybeDestroy ', dbgs(FChromiumState), ' HasWin:',dbgs(FBrowserWindow<>nil), '    ', ptruint(self)]);

  if FWrapperState in [wsWaitingForClose, wsSentCloseEventAfterWait] then
    FWrapperState := wsDestroyAfterWait;

  if FChromiumState = csNoBrowser then
    Destroy;
end;

constructor TChromiumWrapper.Create(AOwner: TLazarusBrowserWindow);
begin
  FBrowserWindow := AOwner;
  FChromiumState := csNoBrowser;
  FWrapperState  := wsNone;

  if not(csDesigning in ComponentState) then
    begin
      FChromium                := TChromium.Create(self);
      FChromium.OnClose        := {$IFDEF FPC}@{$ENDIF}WebBrowser_OnClose;
      FChromium.OnBeforeClose  := {$IFDEF FPC}@{$ENDIF}WebBrowser_OnBeforeClose;
      FChromium.OnAfterCreated := {$IFDEF FPC}@{$ENDIF}WebBrowser_OnAfterCreated;
      {$IFDEF LINUX}
      // This is a workaround for the CEF issue #2026. Read below for more info.
      FChromium.OnGotFocus     := {$IFDEF FPC}@{$ENDIF}WebBrowser_OnGotFocus;
      {$ENDIF}
    end;

  (* Owner = MainForm
     When MainForm closes/destroys all browsers must be closed.

     However if any ChromiumWrapper (with closed browser) was not destroyed,
     then it will go with the MainForm.
     This could happen, if user-code wanted to Destroy the TLazarusBrowserWindow
     in an OnBrowserClosed event, but prior destruction of the TLazarusBrowserWindow
     cancelled the event.
  *)
  inherited Create(Application.MainForm);
end;

destructor TChromiumWrapper.Destroy;
begin
  DebugLn(['### DESTROY  ', dbgs(FChromiumState), ' HasWin:',dbgs(FBrowserWindow<>nil), '    ', ptruint(self)]);

  (* This only happens if we get destroyed by the Owner, which is the App.MainForm
     Any other code should call MaybeDestroy.
  *)
  if FChromiumState <> csNoBrowser then
    WaitForBrowserClosed;

  inherited Destroy;
  Application.RemoveAsyncCalls(Self);
end;

function TChromiumWrapper.CreateBrowser: boolean;
begin
  DebugLnEnter(['>>> CreateBrowser ++ ', dbgs(FChromiumState), ' HasWin:',dbgs(FBrowserWindow<>nil), '    ', ptruint(self)]);
  if FChromiumState <> csNoBrowser then
    exit(False);

  FChromiumState := csCreatingBrowser;
  Result := FChromium.CreateBrowser(FBrowserWindow, '');
  if Result then begin
    if FChromium.Initialized then
      DoCreated(0);
  end
  else begin
    FChromiumState := csNoBrowser;
  end;
  DebugLnExit(['>>> CreateBrowser ++ ', dbgs(FChromiumState), ' HasWin:',dbgs(FBrowserWindow<>nil), '    ', ptruint(self)]);
end;

procedure TChromiumWrapper.LoadURL(aURL: ustring);
begin
  FLoadUrl := '';
  if FChromiumState = csHasBrowser then
    FChromium.LoadURL(aURL)
  else
    FLoadUrl := aURL;
end;

procedure TChromiumWrapper.CloseBrowser(aForceClose: boolean);
begin
  DebugLnEnter(['>>> == CloseBrowser  ', dbgs(FChromiumState), ' HasWin:',dbgs(FBrowserWindow<>nil), '    ', ptruint(self)]);
  if FChromiumState = csCreatingBrowser then begin
    FChromiumState := csCloseAfterCreate;
  end
  else
  if FChromiumState in [csHasBrowser] then
  begin
    FChromiumState := csClosingBrowser;
    FChromium.CloseBrowser(aForceClose);
  end;
  DebugLnExit(['<<< == CloseBrowser  ', dbgs(FChromiumState), ' HasWin:',dbgs(FBrowserWindow<>nil), '    ', ptruint(self)]);
end;

function TChromiumWrapper.IsClosed: boolean;
begin
  Result := FChromiumState = csNoBrowser;
end;

procedure TChromiumWrapper.WaitForBrowserClosed;
begin
  DebugLnEnter(['>>> == WaitCloseBrowser  ', dbgs(FChromiumState), ' HasWin:',dbgs(FBrowserWindow<>nil), '    ', ptruint(self)]);
  if FChromiumState = csNoBrowser then
    exit;
  if FChromiumState <> csClosingBrowser then
    CloseBrowser(True);

  FWrapperState := wsWaitingForClose;
  while FChromiumState <> csNoBrowser do begin
    Application.ProcessMessages;
    if GlobalCEFApp.ExternalMessagePump then
      GlobalCEFApp.DoMessageLoopWork;
    sleep(5);
  end;
  DebugLnExit(['<<< == WaitCloseBrowser  ', dbgs(FChromiumState), ' HasWin:',dbgs(FBrowserWindow<>nil), '    ', ptruint(self)]);

  if (FBrowserWindow <> nil) and
     (FWrapperState = wsSentCloseEventAfterWait)
  then
    Application.QueueAsyncCall(@FBrowserWindow.DoOnClosed, 0);

  if FWrapperState = wsDestroyAfterWait then
    Destroy
  else
    FWrapperState := wsNone;
end;

{ TLazarusBrowserWindow }

procedure TLazarusBrowserWindow.DoCreateBrowser(Sender: TObject);
begin
  FTimer.Enabled := False;
  if FChromiumWrapper.CreateBrowser then begin
    FreeAndNil(FTimer);
  end
  else begin
    if GlobalCEFApp.ExternalMessagePump then
      GlobalCEFApp.DoMessageLoopWork;

    FTimer.Interval := 100;
    FTimer.Enabled  := True;
  end;
end;

function TLazarusBrowserWindow.GetChromium: TChromium;
begin
  Result := FChromiumWrapper.FChromium;
end;

procedure TLazarusBrowserWindow.CreateHandle;
begin
  inherited CreateHandle;
  if not (csDesigning in ComponentState) then begin
    (* On Windows we can create the browser immediately.
       But at least on Linux, we need to wait
    *)
    FTimer := TTimer.Create(Self);
    FTimer.Interval := 20;
    FTimer.OnTimer := @DoCreateBrowser;
    FTimer.Enabled := True;
  end;
end;

procedure TLazarusBrowserWindow.DestroyHandle;
begin
  if FTimer <> nil then
    FreeAndNil(FTimer);

  if (GlobalCEFApp = nil) or // FCanClose or
     (FChromiumWrapper.FChromiumState = csNoBrowser) or
     (csDesigning in ComponentState)
  then begin
DebugLn(['DestroyHandle']);
    inherited DestroyHandle;
    exit;
  end;

  DebugLnEnter(['>>> DestroyHandle']);
  {$IFDEF MACOSX}
  inherited DestroyHandle;
  FChromiumWrapper.CloseBrowser(True);
  {$ELSE}
  Hide;
  FChromiumWrapper.WaitForBrowserClosed;
  inherited DestroyHandle;
  {$ENDIF}
DebugLnExit(['<<< DestroyHandle']);
end;

procedure TLazarusBrowserWindow.RealizeBounds;
begin
  inherited RealizeBounds;

  if not (csDesigning in ComponentState) and HandleAllocated then
    Chromium.NotifyMoveOrResizeStarted;
end;

procedure TLazarusBrowserWindow.DoEnter;
begin
  inherited DoEnter;
  If not(csDesigning in ComponentState) then Chromium.SetFocus(True);
end;

procedure TLazarusBrowserWindow.DoExit;
begin
  inherited DoExit;
  if not(csDesigning in ComponentState) then
    Chromium.SendCaptureLostEvent;
end;

procedure TLazarusBrowserWindow.DoOnCreated;
begin
  {$IFDEF FPC}{$IFDEF LINUX}
  Chromium.UpdateXWindowVisibility(Visible);
  Chromium.UpdateBrowserSize(Left, Top, Width, Height);
  {$ENDIF}{$ENDIF}
  if Assigned(FOnBrowserCreated) then
    FOnBrowserCreated(Self);
end;

procedure TLazarusBrowserWindow.DoOnClosed(Data: PtrInt);
begin
  if (not(csDestroying in ComponentState)) and
     Assigned(FOnBrowserClosed)
  then
    FOnBrowserClosed(Self);
end;

procedure TLazarusBrowserWindow.DoOnFocus(Data: PtrInt);
begin
  SetFocus;
end;

constructor TLazarusBrowserWindow.Create(AOwner: TComponent);
begin
  FCanClose := True;
  FChromiumWrapper := TChromiumWrapper.Create(Self);
  inherited Create(AOwner);
end;

destructor TLazarusBrowserWindow.Destroy;
begin
DebugLnEnter(['>>### Destroy (LazWinBr)   ',ptruint(FChromiumWrapper)]);
  inherited Destroy;
  FChromiumWrapper.MaybeDestroy;
  Application.RemoveAsyncCalls(Self);
DebugLnExit(['<<### Destroy  (LazWinBr)']);
end;

procedure TLazarusBrowserWindow.CloseBrowser(aForceClose: boolean);
begin
  FChromiumWrapper.CloseBrowser(aForceClose);
end;

procedure TLazarusBrowserWindow.WaitForBrowserClosed;
begin
  FChromiumWrapper.WaitForBrowserClosed;
end;

function TLazarusBrowserWindow.IsClosed: boolean;
begin
  Result := FChromiumWrapper.IsClosed;
end;

procedure TLazarusBrowserWindow.LoadURL(aURL: ustring);
begin
  FChromiumWrapper.LoadURL(aURL);
end;

{$IFDEF FPC}

function HasClosingBrowsersWindows: Boolean;
var
  i: Integer;
begin
  Result := False;
  i := Application.ComponentCount - 1;
  while i >= 0 do begin
    if (Application.Components[i] is TChromiumWrapper) and
       not(TChromiumWrapper(Application.Components[i]).IsClosed)
    then
      exit(True);
    dec(i);
  end;
end;

procedure Register;
begin
  {$I res/tlazarusbrowserwindow.lrs}
  RegisterComponents('Chromium', [TLazarusBrowserWindow]);
end;
{$ENDIF}

end.

