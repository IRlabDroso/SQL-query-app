import { Component,Input, OnInit, AfterContentChecked,OnChanges,ViewChild } from '@angular/core';
import { Injectable } from '@angular/core';
import { format } from 'sql-formatter';
import {FormControl} from '@angular/forms';
import {Observable} from 'rxjs';
import {map, startWith} from 'rxjs/operators';
import { HttpClient, HttpParams } from '@angular/common/http';
import { HttpHeaders } from '@angular/common/http';
import { ApiService } from '../app.service';
import {FormBuilder, Validators} from "@angular/forms";
import {saveAs} from "file-saver";
import {ClipboardModule} from '@angular/cdk/clipboard';

interface selectors {
  value: string;
  viewValue: string;
}
export interface User {
  name: string;
}

function delay(ms: number) {
    return new Promise( resolve => setTimeout(resolve, ms) );
}

@Component({
  selector: 'app-conditions',
  templateUrl: './conditions.component.html',
  styleUrls: ['./conditions.component.css']
})
export class ConditionsComponent {

  constructor(private http: HttpClient, private apiService: ApiService){ }

  sql_string = "SELECT * FROM Condition_info WHERE OR IN "
  Query = format(this.sql_string, { language: 'mysql' })

  // OR //
  myControl = new FormControl<string | User>('');
  //OR_options : User[] = [{name: 'DmOR1'}, {name: 'DmOR2'}, {name: 'DmOR1'},{name: 'AgOR14'}];
  OR_options: User[] = [{name: 'DmOR1'}];
  filteredOR!: Observable<User[]>;
  selectedOR: any=null;

  // Driver //
  myControl_Driver = new FormControl<string | User>('');
  //Driver_options : User[] = [{name: 'ORCO'}, {name: 'DmOR2'}, {name: 'DmOR1'},{name: 'AgOR14'}];
  Driver_options: User[] = [];
  filteredDriver!: Observable<User[]>;
  selectedDriver: any=null;

  // Reporter //
  myControl_Reporter = new FormControl<string | User>('');
  //Reporter_options : User[] = [{name: 'GCaMP7F'}, {name: 'GFP'}];
  Reporter_options: User[] = [];
  filteredReporter!: Observable<User[]>;
  selectedReporter: any=null;

  // Plot selection //
  myControl_Plot = new FormControl<string | User>('');
  Plot_options : User[] = [{name: 'plot by conditions'}, {name: 'plot by antennas'}];
  selectedPlot= {name: 'plot by conditions'} as User;



  ngOnInit() {
    this.apiService.getORs().subscribe(levelOBJ => {
      for (let value of levelOBJ) {
        this.OR_options.push({name: value})
      }
      this.OR_options = this.OR_options
      console.log("test")
      console.log(this.OR_options)
    });
    this.apiService.getDrivers().subscribe(levelDriverOBJ => {
      for (let value of levelDriverOBJ) {
        this.Driver_options.push({name: value})

      }
    });
    this.apiService.getReporters().subscribe(levelOBJ => {
      for (let value of levelOBJ) {
        this.Reporter_options.push({name: value as string})
      }
    });

  }



  ngAfterContentChecked() {
   this.filteredOR = this.myControl.valueChanges.pipe(
        startWith(''),
        map(value => {
          const name = typeof value === 'string' ? value : value?.name;
          return name ? this._filter(name as string) : this.OR_options.slice();
        }),

  );

  this.filteredDriver = this.myControl_Driver.valueChanges.pipe(
        startWith(''),
        map(value => {
          const name = typeof value === 'string' ? value : value?.name;
          return name ? this._filterDriver(name as string) : this.Driver_options.slice();
        }),
  );



  this.filteredReporter = this.myControl_Reporter.valueChanges.pipe(
        startWith(''),
        map(value => {
          const name = typeof value === 'string' ? value : value?.name;
          return name ? this._filterReporter(name as string) : this.Reporter_options.slice();
        }),
  );

  }


  // functions

  setSelectedOR(selectedOR: any){
    this.apiService.getQuery(this.selectedOR.name,this.selectedDriver,this.selectedReporter).subscribe(QueryOBJ => {
    this.Query = QueryOBJ
    })
    //this.Query = format(this.sql_string[0], { language: 'mysql' })
  }

  setSelectedDriver(selectedDriver: any){
    this.apiService.getQuery(this.selectedOR.name,this.selectedDriver.name,this.selectedReporter).subscribe(QueryOBJ => {
    this.Query = QueryOBJ
    })

  }

  setSelectedReporter(selectedReporter: any){
    this.apiService.getQuery(this.selectedOR.name,this.selectedDriver.name,this.selectedReporter.name).subscribe(QueryOBJ => {
    this.Query = QueryOBJ
    })
  }

  setSelectedPlot(selectedPlot: any){
    console.log(this.selectedPlot)
    if(this.selectedPlot == this.Plot_options[0]){
      this.imagePath = "/assets/images/samescale_summary_1.png/"
    }else if (this.selectedPlot == this.Plot_options[1]){
      this.imagePath = "/assets/images/samescale_summary_2.png/"
    }
  }

  displayFn(user: User): string {
    return user && user.name ? user.name : '';
  }

  private _filter(name: string): User[] {
    const filterValue = name.toLowerCase();
    console.log(this.OR_options)
    return this.OR_options.filter(option => option.name.toLowerCase().includes(filterValue));
  }
  private _filterDriver(name: string): User[] {
    const filterValue = name.toLowerCase();
    console.log(this.Driver_options)
    return this.Driver_options.filter(option => option.name.toLowerCase().includes(filterValue));
  }
  private _filterReporter(name: string): User[] {
    const filterValue = name.toLowerCase();

    return this.Reporter_options.filter(option => option.name.toLowerCase().includes(filterValue));
  }

  // Button


  table: any= null;
  imagePath: any = null;
  query: any=null;
  OR1 = "DmOR59b"
  Driver1 = "test"
  Reporter1 = "gasd"



  table_formatted: any=null;
  table_raw: any=null;
  postSamescale_plot() {
    this.apiService.getQuery(this.OR1,this.Driver1,this.Reporter1).subscribe(QueryOBJ => {
    this.query = QueryOBJ
    });

    this.apiService.getDataTable(this.OR1,this.Driver1,this.Reporter1).subscribe((QueryOBJ) => {
	  this.table_formatted = QueryOBJ
    })



    this.imagePath = "/assets/images/samescale_summary_1.png/"
    console.log(this.query)
  }

  displayedColumns: string[] = ['short_cond_names', 'odorant','max_zscore'];

  ConvertToCSV(objArray: any = null) {
            var array = typeof objArray != 'object' ? JSON.parse(objArray) : objArray;
            var str = '';

            for (var i = 0; i < array.length; i++) {
                var line = '';
                for (var index in array[i]) {
                    if (line != '') line += ','

                    line += array[i][index];
                }

                str += line + '\r\n';
            }

            return str;
        }

  getDataTable(){
    this.apiService.getDataTable(this.OR1,this.Driver1,this.Reporter1).subscribe((QueryOBJ) => {
	  this.table_formatted = QueryOBJ
	  console.log(this.table_formatted)
	  const new_table = 'max_zscore,pulse_id,antenna_id,condition,odorant,OR,short_cond_name\n' + this.ConvertToCSV(QueryOBJ)
	  const data: Blob = new Blob([new_table], {
	      type: "text/csv;charset=utf-8"
    });
	  saveAs(data,"formatted_data.csv")
    })
  }

  getDataTableRaw(){
    this.apiService.getDataTableRaw(this.OR1,this.Driver1,this.Reporter1).subscribe(QueryOBJ => {
	  this.table_raw = this.ConvertToCSV(QueryOBJ)
	  const new_table = 'max_zscore,pulse_id,antenna_id,condition,odorant,OR,short_cond_name\n' + this.table_formatted
	  const data: Blob = new Blob([new_table], {
	      type: "text/csv;charset=utf-8"
    });
	  saveAs(data,"raw_data.csv")
    })
  }



  SQLSelectedChange(data: Event){
    console.log("entered here")
    this.sql_string = 'SELECT * FROM Condition_Info WHERE OR IN ' + data
    this.Query = format(this.sql_string, { language: 'mysql' })
  }



}
