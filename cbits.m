#import "AppKit/AppKit.h"
#import "AppKit/NSStatusBar.h"

#include <unistd.h>

void release(id o)
{
    [o release];
}

void freeHaskellFunPtr(void (*ptr)(void));

@interface IOAction : NSObject
{
    @public void (*ioaction)(void);
}

- (void) callIOAction;
- (void) dealloc;
@end

@implementation IOAction

- (void) callIOAction
{
    self->ioaction();
}

- (void) dealloc
{
    freeHaskellFunPtr(self->ioaction);
    [super dealloc];
}

@end

/******************************************************/

@interface MyEvent : NSEvent
{
}

- (void) dealloc;
@end
@implementation MyEvent
- (void) dealloc
{
    [super dealloc];
}
@end

@interface MyMenuItem : NSMenuItem
{
}

- (void) dealloc;
@end

@implementation MyMenuItem

- (void) dealloc
{
    [super dealloc];
}

@end

@interface MyMenu : NSMenu
{
}
- (void) dealloc;
@end

@implementation MyMenu
- (void) dealloc
{
    [super dealloc];
}
@end

/******************************************************/

void sendEvent(void)
{
    NSEvent *e = [MyEvent otherEventWithType: NSApplicationDefined location: NSZeroPoint modifierFlags: 0 timestamp: 0 windowNumber: 0 context: nil subtype: 12 data1: 0 data2: 0];
    [NSApp sendEvent: e];
    [e release];
}

void initApp(void)
{
    [NSApplication sharedApplication];
}

NSStatusItem *newStatusItem(void)
{
    return [[NSStatusBar systemStatusBar] statusItemWithLength:NSVariableStatusItemLength];
}

NSString *newNSString(char *str)
{
    return [[NSString string] initWithUTF8String: str];
}

void setTitle(NSStatusItem *si, char *title)
{
    NSString *t = newNSString(title);
    si.button.title = t;
    [t release];
}

void runApp(void (*ptr)(void))
{
    [NSEvent addLocalMonitorForEventsMatchingMask: NSEventMaskApplicationDefined handler: ^NSEvent * _Nullable (NSEvent *e){
        ptr();
        return nil;
    }];
    [NSApp run];
}

void sendTerminate(void)
{
    [NSApp terminate: nil];
}

MyMenu *newMenu(char *title)
{
    NSString *t = newNSString(title);
    MyMenu *m = [[MyMenu alloc] initWithTitle: t];
    [t release];
    return m;
}

MyMenuItem *newMenuItem(char *title)
{
    NSString *t = newNSString(title);
    MyMenuItem *mi = [[MyMenuItem alloc] initWithTitle: t action: NULL keyEquivalent: @""];
    [t release];
    return mi;
}

void assignAction(MyMenuItem *mi, void (*ptr)(void))
{
    IOAction *action = [IOAction alloc];
    action->ioaction = ptr;
    mi.target = action;
    mi.action = @selector(callIOAction);
    mi.representedObject = action;
    [action release];
}

void assignSubMenu(MyMenuItem *mi, MyMenu *m)
{
    mi.submenu = m;
}

NSMenuItem *newSeparator(void)
{
    return [NSMenuItem separatorItem];
}

void addMenuItem(MyMenu *m, NSMenuItem *i)
{
    [m addItem: i];
}

void setStatusItemMenu(NSStatusItem *si, MyMenu *m)
{
    si.menu = m;
}

