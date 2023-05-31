import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { HttpErrorResponse, HttpResponse } from '@angular/common/http';

import { Observable, throwError } from 'rxjs';
import { catchError, retry } from 'rxjs/operators';

export interface Config {
  RUrl: string;
  textfile: string;
  date: any;
}

@Injectable()
export class ConfigService {
  configUrl = 'assets/config.json';

  constructor(private http: HttpClient) { }

  getConfig() {
    return this.http.get<Config>(this.configUrl);
  }

  config: Config | undefined;
  showConfig() {
    this.configService.getConfig()
      .subscribe((data: Config) => this.config = {
          RUrl: data.RUrl,
          textfile:  data.textfile,
          date: data.date,
      });
  }

}

