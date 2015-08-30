unit HS_FMXHints;

// 04.04.2013
// (c) copyright Harry Stahl Software, www.hastasoft.de or  www.devpage.de
// Usable for DXE2 to DXE8, for newer Versions test it by your self
// You can use this software free, but use it on your own risk

// ********************** IMPORTANT *******************************
// 1. Set this Unit AT THE END in the unitlist you use, otherwise the hack will not work
// 2. If you use hints not only in Mainform, call in the other forms "HideHint"
//    in OnFormDestroy-Event to avoid destroying the AcallHint-Object by the
//    Form (because it is, when visible, a child of the Hinted-Parent-Control).

// Restrictions: Only Controls, where "Hitset=true" is set, can show hints,
//               So for TLabel you have to set Hitset = True if you want to use a hint here

// ************************* USAGE *********************************
// SetHintSetting (TimeBeforeShow, TimeToShow: Integer; DynamicShow: Boolean; TC: TAlphaColor);
// --> Modify the hintsettions
// HideHint; (in other Units than the mainform, in OndestroyEvent);
// SetAhint (Button1, 'This is the hint');
// SetActionHint (Button2, FileExit1); // Where Fileexit1 is a sandardaction ( >= XE3 )
// SetActionHint (Button2, MyAction1); // Where MyAction1 is a user-action ( >= XE3 )

// ********************** UPDATES **********************************
// Update: 26.08.2014
// * Now all Controls can have a hint
// Update: 22.05.2015
// * If A hint has no text, the hint box will not be displayed
// * New Procedure: SetActionhint ( >= XE3, because FMX has in XE2 no actions)
//   Use the hinttext from Action.hint, that you can edit and save with the objectinspector

interface

{$IFDEF VER230} {$DEFINE USETYPESUNIT} {$ENDIF} // XE2
{$IFDEF VER240} {$DEFINE USETYPESUNIT} {$ENDIF} // XE3
{$IFDEF VER250} {$UNDEF USETYPESUNIT} {$ENDIF}// NOT in XE4
{$IFDEF VER260} {$UNDEF USETYPESUNIT} {$ENDIF}// NOT in XE5

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Effects,  FMX.Controls, FMX.Forms, FMX.Graphics,

  {$IF FireMonkeyVersion > 17.0} FMX.StdCtrls, {$IFEND}
  {$IF FireMonkeyVersion >= 17.0} FMX.ActnList, {$IFEND} // XE2 has no actions

  FMX.Objects;

type
  {$IFDEF USETYPESUNIT} // XE2-XE3
  TControl = class (FMX.Types.TControl);
  {$ELSE}
  TControl = class (FMX.Controls.TControl);
  {$ENDIF}

  procedure HintEnter (Self, Sender: TObject);
  procedure HintExit (Self, Sender: TObject);
  procedure HintTimer (Self, Sender: TObject);

  procedure SetHintSetting (TimeBeforeShow, TimeToShow: Integer; DynamicShow: Boolean; TC: TAlphaColor);
  procedure SetAHint (ic: TObject; txt: string);
  {$IF FireMonkeyVersion >= 17.0}
  procedure SetActionHint (ic: TObject; AnAction: TcustomAction);
  {$IFEND}
  procedure HideHint;

implementation

var
  ACallHint: TCalloutRectangle;
  Lab, lbDummy: TLabel; TT: TTimer; DynamicTime: Boolean;
  TimeBefore, TimeShow: Integer;
  EventEnter, EventExit, EventTimer: TNotifyEvent;

Procedure HideHint;
begin
  if ACallHint <> NIL then begin
    ACallHint.Visible := false;
    ACallHint.Parent := NIL;
  end;
end;

procedure SetHintSetting (TimeBeforeShow, TimeToShow: Integer; DynamicShow: Boolean; TC: TAlphaColor);
begin
  TimeBefore := TimeBeforeShow;
  TimeShow := TimeToShow;
  DyNamicTime := DyNamicShow;
  ACallHint.Fill.Color := TC;
end;

procedure HintTimer (Self, Sender: TObject);
begin
  TT.Enabled := false;

  if ACallHint.Visible = false then begin
    ACallHint.Visible := True;
    ACallHint.BringToFront;
    TT.Enabled := True;

    if DynamicTime then begin
      TT.Interval := TimeShow + 45 * (Length (lab.Text));
    end else begin
      TT.Interval := TimeShow;
    end;

  end else begin
    ACallHint.Visible := false;
    ACallHint.Parent := NIL;
  end;
end;

procedure InitHintSetting;
var
  ShadowEffect1: TShadowEffect;
begin
  TMethod (EventEnter).code := @HintEnter;
  TMethod (EventExit).code := @HintExit;
  TMethod (EventTimer).code := @HintTimer;
  //TMethod (Event2).data := NIL; // Not needed here

  ACallHint:= TCalloutRectangle.Create(NIL);
  ACallHint.Width := 200; ACallHint.Height := 35;
  ACallHint.CalloutOffset := 8;
  ACallHint.Fill.Color := TAlphaColorRec.Beige;

  {$IF FireMonkeyVersion <= 17.0} //XE3-XE3
  lbDummy := TLabel.Create (ACallHint); lbDummy.Parent := ACallHint; lbDummy.Align := TAlignLayout.alTop; lbDummy.Height := 10;
  {$IFEND}

  ShadowEffect1:= TShadowEffect.Create(ACallHint);
  ShadowEffect1.Parent := ACallHint;

  Lab := TLabel.Create (ACallHint);
  Lab.Parent := ACallHint;
  Lab.Align := TAlignLayout.Client;
  Lab.TextAlign := TTextAlign.Center;
  Lab.VertTextAlign := TTextAlign.Center;
  {$IFDEF VER230} // XE2
  {$ELSE}
  Lab.StyledSettings := [];
  {$ENDIF}

  with Lab.Margins do begin Top := 10; left := 5; right := 5; end;

  TimeBefore := 800; TimeShow := 2000; // Standard-Init-Settings

  TT := TTImer.Create(NIL);
  TT.Interval := TimeBefore;
  TT.Ontimer := EventTimer;
end;

procedure SetAHint (ic: TObject; txt: string);
begin
  TControl (ic).Hint := txt;
  if not Assigned (TControl (ic).OnMouseEnter) then TControl (ic).OnMouseEnter := EventEnter;
  if not Assigned (TControl (ic).OnMouseLeave) then TControl (ic).OnMouseLeave := EventExit;
end;

{$IF FireMonkeyVersion >= 17.0}
procedure SetActionHint (ic: TObject; AnAction: TCustomAction);
begin
  if AnAction <> NIL then begin
    if pos ('|', AnAction.Hint) <> 0 then TControl (ic).Hint := copy (AnAction.Hint, 1, pos ('|', AnAction.Hint)-1) else
      TControl (ic).Hint := AnAction.Hint;
    if not Assigned (TControl (ic).OnMouseEnter) then TControl (ic).OnMouseEnter := EventEnter;
    if not Assigned (TControl (ic).OnMouseLeave) then TControl (ic).OnMouseLeave := EventExit;
  end;
end;
{$IFEND}

procedure HintEnter(Self, Sender: TObject);
var
  aw: Extended; aTop:Extended; tp: TPointF; DForm: TForm;

  function GetFormParent (o: TFMXObject): TFMXObject;
  begin
    Result := o;
    while Result.Parent <> NIL do begin
      Result := Result.Parent;
      if Result is TForm then break;
    end;

    DForm := TForm(Result);
  end;

begin
  ACallhint.Parent := GetFormParent (TControl (sender).Parent);

  Lab.text := TControl (sender).hint;

  if Lab.text = '' then begin
    if Assigned(TControl(Sender).Action) then
      Lab.Text := TAction(TControl(Sender).Action).Hint;
    if Lab.Text = '' then
      exit; // Exit here, if the hint has no content
  end;

  TP := TControl (sender).LocalToScreen(PointF (TControl (sender).position.X, TControl (sender).Position.Y));
  TP := DForm.ScreenToClient(tp);
  ACallHint.position.X := Tp.X-TControl (sender).Position.x;
  ACallHint.Position.Y := TP.Y + TControl (sender).Height+2 - TControl (sender).Position.Y;
  ATop := ACAllHint.Position.Y - TControl (sender).height - 2;

  aw := lab.Canvas.TextWidth(lab.text) + 20; if aw < 100 then aw := 100;

  if aw > 200 then begin
    aw := 200;
    ACallHint.height := (round ((aw / 200)) + 1) * 38;
  end else begin
    ACallHint.height := 35;
  end;

  ACallHint.Width := aw;

  if ACallHint.Position.X + ACallHint.width > DForm.ClientWidth then begin
    ACallHint.Position.X := DForm.ClientWidth - ACallHint.width-5;
    ACallHint.CalloutOffset := ACallHint.width - 40;
  end else begin
    ACallHint.CalloutOffset := 8;
  end;

  // CAllHint out of sight?
  if ACallHint.Position.Y + ACallHint.Height > DForm.ClientHeight then begin
    ACAllHint.Position.y := ACAllHint.Position.y - ACAllHint.Height - TControl (sender).height;
    ACAllHint.CalloutPosition := TCalloutPosition.Bottom;
    with Lab.Margins do begin Top := 0; bottom := 10; left := 5; right := 5; end;
    {$IF FireMonkeyVersion <= 17.0} // Workaround XE2-XE3
    lbDummy.Align := TAlignLayout.alBottom;
    {$IFEND}
  end else begin
    with Lab.Margins do begin Top := 10; left := 5; right := 5; bottom :=0; end;
    ACAllHint.CalloutPosition := TCalloutPosition.Top;
    {$IF FireMonkeyVersion <= 17.0} // Workaround XE2-XE3
    lbDummy.Align := TAlignLayout.alTop;
    {$IFEND}
  end;

  TT.Interval := TimeBefore;
  TT.Enabled := True;
end;

procedure HintExit(Self, Sender: TObject);
begin
  ACallHint.Visible := false;
  ACallHint.Parent := NIL;
  TT.Enabled := false;
end;

Initialization
  InitHintSetting;
Finalization
  ACallHint.Parent := NIL;
  ACallHint.Free;
  TT.Free;
end.
