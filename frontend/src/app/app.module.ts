import { NgModule } from '@angular/core';
import { BrowserModule } from '@angular/platform-browser';
import {MatSidenavModule} from '@angular/material/sidenav';
import { RouterModule, Routes} from '@angular/router';
import { format } from 'sql-formatter';


import { AppRoutingModule } from './app-routing.module';
import { AppComponent } from './app.component';
import { ConditionsComponent } from './conditions/conditions.component';
import { TrialsComponent } from './trials/trials.component';
import { SideNavComponent } from './side-nav/side-nav.component';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import {MatSelectModule} from '@angular/material/select';
import {MatCardModule} from '@angular/material/card';
import {MatFormFieldModule} from '@angular/material/form-field';
import {MatButtonModule} from '@angular/material/button';
import {MatListModule} from '@angular/material/list';
import {MatToolbarModule} from '@angular/material/toolbar';
import {MatAutocompleteModule} from '@angular/material/autocomplete';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import { MatInputModule } from '@angular/material/input';
import {platformBrowserDynamic} from '@angular/platform-browser-dynamic';
import { ConfigComponent } from './config/config.component';
import { HttpClientModule, HttpClient } from '@angular/common/http';
import { AgGridModule } from 'ag-grid-angular';
import {MatTabsModule} from '@angular/material/tabs';
import {MatTableModule} from '@angular/material/table';
import {MatPaginator, MatPaginatorModule} from '@angular/material/paginator';
import {ClipboardModule} from '@angular/cdk/clipboard';
import { NgbModule } from '@ng-bootstrap/ng-bootstrap';

const routes: Routes = [
  {
    path: '',
    component: ConditionsComponent,
  },
  {
    path: 'conditions',
    component: ConditionsComponent,
  },
  {
    path: 'trials',
    component: TrialsComponent,
  }

];


@NgModule({
  declarations: [
    AppComponent,
    ConditionsComponent,
    TrialsComponent,
    SideNavComponent,
    ConfigComponent,

  ],
  imports: [
    BrowserModule,
    HttpClientModule,
    AppRoutingModule,
    RouterModule.forRoot(routes),
    MatSidenavModule,
    BrowserAnimationsModule,
    MatCardModule,
    MatSelectModule,
    MatFormFieldModule,
    MatButtonModule,
    MatListModule,
    MatToolbarModule,
    MatAutocompleteModule,
    ReactiveFormsModule,
    MatInputModule,
    AgGridModule,
    MatTabsModule,
    MatTableModule,
    MatPaginatorModule,
    ClipboardModule,
    NgbModule



  ],
  exports: [RouterModule],
  providers: [],
  bootstrap: [AppComponent]
})
export class AppModule { }
