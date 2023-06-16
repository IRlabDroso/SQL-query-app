import { ComponentFixture, TestBed } from '@angular/core/testing';

import { BothqueryComponent } from './bothquery.component';

describe('BothqueryComponent', () => {
  let component: BothqueryComponent;
  let fixture: ComponentFixture<BothqueryComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ BothqueryComponent ]
    })
    .compileComponents();

    fixture = TestBed.createComponent(BothqueryComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
