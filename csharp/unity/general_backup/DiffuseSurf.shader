Shader "Custom/Diffuse Surf" {
	Properties {
		_MyColor ("My Color", Color) = (1, 0, 0, 1)
		_MySpeed ("Speed", float) = 1
	}

    SubShader {
      Tags { "RenderType" = "Opaque" }
      CGPROGRAM
      #pragma surface surf Lambert
      float4 _MyColor;
      float _MySpeed;
      struct Input {
          float4 color : COLOR;
      };
      void surf (Input IN, inout SurfaceOutput o) {
      	  o.Albedo = _MyColor;
      	  o.Albedo.r = sin(_Time[1] * _MySpeed);
      	  o.Albedo.g = cos(_Time[1] * _MySpeed);
      }
      ENDCG
    }
    Fallback "Diffuse"
  }