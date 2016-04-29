support_L = 53;
support_H = 8;
support_width = 15;
top_width = 2;
wall_width = 2;

pin_dist = 12;
pin_h = 6.5;


module pin(pin_height = 1.5, pin_d = 1.5){
  rotate([90, 0, 0])
  cylinder(h = pin_height, d = pin_d, $fn = 4);
}

linear_extrude(height = support_width, center = true)
polygon( points=[[0,0],
                 [support_L/2,0],
                 [support_L/2, -support_H],
                 [support_L/2 + wall_width, -support_H],
                 [support_L/2 + wall_width, top_width],
                 [-support_L/2 - wall_width, top_width],
                 [-support_L/2 - wall_width, -support_H],
                 [-support_L/2, -support_H],
                 [-support_L/2,0],
                ]);

translate([-support_L/2, -pin_h, -pin_dist/2])
rotate([0, 0, 90])
pin();

translate([-support_L/2, -pin_h, pin_dist/2])
rotate([0, 0, 90])
pin();

translate([support_L/2, -pin_h, -pin_dist/2])
rotate([0, 0, -90])
pin();

translate([support_L/2, -pin_h, pin_dist/2])
rotate([0, 0, -90])
pin();