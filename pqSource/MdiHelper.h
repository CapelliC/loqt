/*
    pqSource     : interfacing SWI-Prolog source files and Qt

    Author       : Carlo Capelli
    E-mail       : cc.carlo.cap@gmail.com
    Copyright (C): 2013,2014 Carlo Capelli

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/

#ifndef MDIHELPER_H
#define MDIHELPER_H

#include "pqSource_global.h"

#include <QMenu>
#include <QLabel>
#include <QMdiArea>
#include <QMainWindow>
#include <QTextCursor>
#include <QMdiSubWindow>
#include <QComboBox>

#include "MruHelper.h"
#include "RowColIndicators.h"

class PQSOURCESHARED_EXPORT MdiHelper : public QMainWindow, protected MruHelper
{
    Q_OBJECT

protected:

    MdiHelper(QWidget *parent = 0, Qt::WindowFlags flags = 0) : QMainWindow(parent, flags) {}
    void setupMdi();

    // from MDI example
    void createActions();
    void createMenus();
    void createToolBars();
    void createStatusBar();

    QMdiSubWindow *findMdiChild(const QString &fileName);

    QMenu *fileMenu;
    QMenu *editMenu;
    QMenu *debugMenu;
    QMenu *windowMenu;
    QMenu *helpMenu;

    QToolBar *fileToolBar;
    QToolBar *editToolBar;
    QToolBar *debugToolBar;
    QToolBar *helpBar;

    QComboBox *queriesBox;

    QAction *newAct;
    QAction *openAct;
    QAction *saveAct;
    QAction *saveAsAct;
    QAction *switchLayoutDirectionAct;
    QAction *switchViewModeAct;
    QAction *exitAct;

    QAction *cutAct;
    QAction *copyAct;
    QAction *pasteAct;

    QAction *findAct;
    QAction *findNextAct;
    QAction *findPreviousAct;
    QAction *replaceAct;

    QAction *makeAct;
    QAction *consultAct;
    QAction *runAct;
    QAction *stopAct;
    QAction *stepInAct;
    QAction *stepOutAct;
    QAction *stepOverAct;
    QAction *toggleBPAct;
    QAction *watchBPAct;

    QAction *closeAct;
    QAction *closeAllAct;
    QAction *tileAct;
    QAction *cascadeAct;
    QAction *nextAct;
    QAction *previousAct;
    QAction *separatorAct;

    QAction *helpStartAct;
    QAction *helpDocAct;
    QAction *aboutAct;
    QAction *aboutQtAct;

    QAction *viewGraphAct;

    inline QMdiArea* mdiArea() const {
        return qobject_cast<QMdiArea*>(centralWidget());
    }

    inline QWidget* activeMdiChild() const {
        const QMdiSubWindow *w = mdiArea()->activeSubWindow();
        return w ? w->widget() : 0;
    }

    template <class T>
    inline T* activeChild() const {
        return qobject_cast<T*>(activeMdiChild());
    }

    template<class T>
    QList<T*> matchingSubWindows(std::function<bool(T*)> match) const
    {
        QList<T*> l;
        foreach (auto w, mdiArea()->subWindowList())
            if (auto s = qobject_cast<T*>(w->widget()))
                if (match(s))
                    l << s;
        return l;
    }

    // associate a string key with a widget
    virtual QString symbol(QWidget *w) {
        // rough approx.
        return w->windowTitle();
    }

    RowColIndicators cursorIndicator;

private slots:
    void updateMenus();
    void updateWindowMenu();
    void setActiveSubWindow(QWidget *window);
    void switchLayoutDirection();
    void switchViewMode();
    void cut();
    void copy();
    void paste();

protected slots:
    void subWindowActivated(QMdiSubWindow *);
    void cursorPositionChanged();
};

#endif // MDIHELPER_H
