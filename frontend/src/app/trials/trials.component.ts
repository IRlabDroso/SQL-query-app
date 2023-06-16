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
  selector: 'app-trials',
  templateUrl: './trials.component.html',
  styleUrls: ['./trials.component.css']
})
export class TrialsComponent {
  constructor(private http: HttpClient, private apiService: ApiService){ }

  sql_string = "SELECT * FROM Condition_info WHERE OR IN "
  Query = format(this.sql_string, { language: 'mysql' })

  // Odorant //
  myControl_Odorant = new FormControl<string | User>('');
  //Reporter_options : User[] = [{name: 'GCaMP7F'}, {name: 'GFP'}];
  Odorant_options: User[] = [];
  filteredOdorant!: Observable<User[]>;
  selectedOdorant: any=null;

  // Dilution //
  myControl_Dilution = new FormControl<string | User>('');
  //Reporter_options : User[] = [{name: 'GCaMP7F'}, {name: 'GFP'}];
  Dilution_options: User[] = [];
  filteredDilution!: Observable<User[]>;
  selectedDilution: any=null;

  // Plot selection //
  myControl_Plot = new FormControl<string | User>('');
  Plot_options : User[] = [{name: 'plot by conditions'}, {name: 'plot by antennas'}];
  selectedPlot= {name: 'plot by conditions'} as User;



  ngOnInit() {
    this.apiService.getOdorants().subscribe(levelOBJ => {
      for (let value of levelOBJ) {
        this.Odorant_options.push({name: value as string})
      }
    });

    this.apiService.getDilutions().subscribe(levelOBJ => {
      for (let value of levelOBJ) {
        this.Dilution_options.push({name: value as string})
      }
    });

  }



  ngAfterContentChecked() {

  this.filteredOdorant = this.myControl_Odorant.valueChanges.pipe(
        startWith(''),
        map(value => {
          const name = typeof value === 'string' ? value : value?.name;
          return name ? this._filterOdorant(name as string) : this.Odorant_options.slice();
        })

  );

  this.filteredDilution = this.myControl_Dilution.valueChanges.pipe(
        startWith(''),
        map(value => {
          const name = typeof value === 'string' ? value : value?.name;
          return name ? this._filterDilution(name as string) : this.Dilution_options.slice();
        }),
  );

  }


  // functions

  setSelectedOdorant(selectedOdorant: any){
    this.apiService.getQuery(this.selectedOdorant.name).subscribe(QueryOBJ => {
    this.Query = QueryOBJ
    });
    this.apiService.getDilutions(this.selectedOdorant.name).subscribe(levelOBJ => {
      for (let value of levelOBJ) {
        this.Dilution_options.push({name: value as string})
      }
    });
  }

  setSelectedDilution(selectedDilution: any){
    this.apiService.getQuery(this.selectedOdorant.name,this.selectedDilution.name).subscribe(QueryOBJ => {
    this.Query = QueryOBJ
    })
  }

  setSelectedPlot(selectedPlot: any){
    console.log(this.selectedPlot)

    if(this.selectedPlot == this.Plot_options[0]){
      console.log("entered here plot")
      this.imagePath = "/assets/images/samescale_summary_1.png/"
    }else if (this.selectedPlot == this.Plot_options[1]){
      this.imagePath = "/assets/images/samescale_summary_2.png/"
    }
  }

  displayFn(user: User): string {
    return user && user.name ? user.name : '';
  }
  private _filterOdorant(name: string): User[] {
    const filterValue = name.toLowerCase();
    return this.Odorant_options.filter(option => option.name.toLowerCase().includes(filterValue));
  }
  private _filterDilution(name: string): User[] {
    const filterValue = name.toLowerCase();
    return this.Dilution_options.filter(option => option.name.toLowerCase().includes(filterValue));
  }

  // Button


  table: any= null;
  imagePath: any = null;
  query: any=null;
  response = "False";



  table_formatted: any=null;
  table_raw: any=null;
  async postSamescale_plot() {
    await this.apiService.getQuery(this.selectedOdorant.name,this.selectedDilution.name).subscribe(QueryOBJ => {
    this.query = QueryOBJ
    });

    await this.apiService.getDataTable(this.selectedOdorant.name,this.selectedDilution.name).subscribe((QueryOBJ) => {
	  this.table_formatted = QueryOBJ
    })

    await this.apiService.getSameScalePlot(this.selectedOdorant.name,this.selectedDilution.name).subscribe((QueryOBJ) => {

    })

    this.imagePath = "/assets/images/samescale_summary_1.png/"
  };

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
    this.apiService.getDataTable(this.selectedOdorant.name,this.selectedDilution.name).subscribe((QueryOBJ) => {
	  this.table_formatted = QueryOBJ
	  const new_table = 'max_zscore,pulse_id,antenna_id,condition,odorant,OR,short_cond_name\n' + this.ConvertToCSV(QueryOBJ)
	  const data: Blob = new Blob([new_table], {
	      type: "text/csv;charset=utf-8"
    });
	  saveAs(data,"formatted_data.csv")
    })
  }

  getDataTableRaw(){
    this.apiService.getDataTableRaw(this.selectedOdorant.name,this.selectedDilution.name).subscribe((QueryOBJ) => {
	  this.table_raw = QueryOBJ
	  const new_table = 'max_zscore,pulse_id,antenna_id,condition,odorant,OR,short_cond_name\n' + this.ConvertToCSV(QueryOBJ)
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
