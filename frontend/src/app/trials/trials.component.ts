import { Component, OnInit, AfterViewInit } from '@angular/core';
import { format } from 'sql-formatter';
import {FormControl} from '@angular/forms';
import {Observable} from 'rxjs';
import {map, startWith} from 'rxjs/operators';

export interface User {
  name: string;
}

@Component({
  selector: 'app-trials',
  templateUrl: './trials.component.html',
  styleUrls: ['./trials.component.css']
})
export class TrialsComponent {
  sql_string = "SELECT * FROM Trial_info WHERE odor IN "
  Query = format(this.sql_string, { language: 'mysql' })


  myControl = new FormControl<string | User>('');
  odors_options : User[] = [{name: 'Methyl Acetate'}, {name: 'Cider'}, {name: 'Water'}];
  filteredOdors!: Observable<User[]>;
  selectedOdor: any;

  myControl_dil = new FormControl<string | User>('');
  Dilutions_options : User[] = [{name: '1'}, {name: '2'}, {name: '3'}];
  filteredDilutions!: Observable<User[]>;
  selectedDil: any;

  ngOnInit() {
    this.filteredOdors = this.myControl.valueChanges.pipe(
      startWith(''),
      map(value => {
        const name = typeof value === 'string' ? value : value?.name;
        return name ? this._filter(name as string) : this.odors_options.slice();
      }),
    );
    this.filteredDilutions = this.myControl_dil.valueChanges.pipe(
      startWith(''),
      map(value => {
        const name = typeof value === 'string' ? value : value?.name;
        return name ? this._filterDil(name as string) : this.Dilutions_options.slice();
      }),
    );

  }

  setSelectedDil(selectedDil: any){
    console.log(selectedDil.name);
    console.log(this.selectedDil);
    this.selectedDil = selectedDil
    this.sql_string = 'SELECT * FROM Trial_info WHERE odor IN ' + this.selectedOdor.name + ' AND dilution = ' + this.selectedDil.name
    this.Query = format(this.sql_string, { language: 'mysql' })
  }
  setSelectedOdor(selectedOdor: any){
    console.log(selectedOdor.name);
    console.log(this.selectedOdor);
    this.selectedOdor = selectedOdor
    if(this.selectedDil == undefined){
    this.sql_string = 'SELECT * FROM Trial_info WHERE odor IN ' + this.selectedOdor
    this.Query = format(this.sql_string, { language: 'mysql' })
    }else{
    this.sql_string = 'SELECT * FROM Trial_info WHERE odor IN ' + this.selectedOdor.name  + ' AND dilution = ' + this.selectedDil.name 
    this.Query = format(this.sql_string, { language: 'mysql' })
    }
  }


  displayFn(user: User): string {
    return user && user.name ? user.name : '';
  }

  private _filter(name: string): User[] {
    const filterValue = name.toLowerCase();

    return this.odors_options.filter(option => option.name.toLowerCase().includes(filterValue));
  }

  private _filterDil(name: string): User[] {
    const filterValue = name.toLowerCase();

    return this.Dilutions_options.filter(option => option.name.toLowerCase().includes(filterValue));
  }
}
